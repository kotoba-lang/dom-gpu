(ns kotoba.wasm.retained-host-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.wasm.abi :as abi]
            [kotoba.wasm.host.retained :as retained]))

(def ops
  (:ops
   (abi/encode-batch
    [[:dom/create-element 1 :main]
     [:dom/set-root 1]
     [:dom/set-attr 1 :style/width 240]
     [:dom/create-element 2 :button]
     [:dom/set-attr 2 :id "run"]
     [:dom/set-attr 2 :style/background "#112233"]
     [:dom/add-event-listener 2 :click 99]
     [:dom/add-event-listener 2 :key-down 100]
     [:dom/create-text 3 "Run"]
     [:dom/append-child 2 3]
     [:dom/append-child 1 2]])))

(defn state []
  (reduce retained/apply-op
          (merge retained/base-state {:width 320})
          ops))

(deftest retained-state-applies-abi-ops
  (let [s (state)]
    (is (= 1 (:root s)))
    (is (= :main (get-in s [:nodes 1 :tag])))
    (is (= [2] (get-in s [:nodes 1 :children])))
    (is (= "run" (get-in s [:nodes 2 :attrs :id])))
    (is (= [99] (get-in s [:listeners 2 :click])))
    (is (= [100] (get-in s [:listeners 2 :key-down])))))

(deftest retained-node-tree-keeps-listeners-for-layout
  (let [tree (retained/node-tree (state) 1)
        button (first (:children tree))]
    (is (= :button (:tag button)))
    (is (= #{:click :key-down} (set (:listeners button))))
    (is (= ["Run"] (:children button)))))

(deftest retained-remove-event-listener-clears-listeners
  ;; Previously entirely unhandled (falling through to the default `state`
  ;; below, a silent no-op) -- a real removeEventListener() call would
  ;; leave :listeners stale, so a node's own node-tree projection would
  ;; keep reporting a listener real dispatch logic elsewhere had already
  ;; stopped calling.
  (let [remove-ops (:ops (abi/encode-batch [[:dom/remove-event-listener 2 :click 99]]))
        s (reduce retained/apply-op (state) remove-ops)]
    (testing "the removed event-type is gone"
      (is (not (contains? (get-in s [:listeners 2]) :click))))
    (testing "the OTHER event-type on the same node is untouched"
      (is (= [100] (get-in s [:listeners 2 :key-down]))))
    (testing "node-tree no longer reports the removed listener"
      (let [tree (retained/node-tree s 1)
            button (first (:children tree))]
        (is (= #{:key-down} (set (:listeners button))))))))

(deftest retained-add-event-listener-supports-multiple-independent-listeners-per-node-and-type
  ;; Real HTML5 addEventListener supports MULTIPLE independent listeners
  ;; for the same (element, event-type) pair -- previously a second
  ;; add-event-listener op here silently overwrote the first via a plain
  ;; assoc-in, only the most-recently-added listener ever registered at
  ;; all. Mirrors kotoba.wasm.dom/add-event-listener's own, already-
  ;; correct multi-listener model.
  (let [ops (:ops (abi/encode-batch [[:dom/add-event-listener 2 :click 100]]))
        s (reduce retained/apply-op (state) ops)]
    (is (= [99 100] (get-in s [:listeners 2 :click]))
        "both handlers survive, in registration order")))

(deftest retained-add-event-listener-registering-the-same-handler-twice-is-idempotent
  ;; Matches real addEventListener semantics for a duplicate registration:
  ;; the SAME handler-id added twice for the same (node, event-type) is a
  ;; no-op, not a duplicate entry.
  (let [ops (:ops (abi/encode-batch [[:dom/add-event-listener 2 :click 99]]))
        s (reduce retained/apply-op (state) ops)]
    (is (= [99] (get-in s [:listeners 2 :click])))))

(deftest retained-remove-event-listener-removes-only-the-matching-handler-leaving-siblings-intact
  ;; The companion bug to the multi-listener add fix above: previously
  ;; remove-event-listener ignored WHICH handler-id was being removed and
  ;; dissoc'd the WHOLE event-type entry -- removing ONE of two click
  ;; listeners on the same node silently killed BOTH. Mirrors
  ;; kotoba.wasm.dom/remove-event-listener's own, already-correct model.
  (let [add-ops (:ops (abi/encode-batch [[:dom/add-event-listener 2 :click 100]]))
        remove-ops (:ops (abi/encode-batch [[:dom/remove-event-listener 2 :click 99]]))
        s (reduce retained/apply-op (state) (concat add-ops remove-ops))]
    (is (= [100] (get-in s [:listeners 2 :click]))
        "only handler 99 is removed; handler 100, registered on the SAME (node, event-type), survives")))

(deftest retained-hit-event-dispatches-to-every-registered-handler-not-just-the-first
  ;; The observable end-to-end consequence of the storage fixes above:
  ;; a real click on an element with TWO independently-registered click
  ;; listeners must produce TWO real dispatch events (one per handler,
  ;; in registration order), not just one.
  (let [add-ops (:ops (abi/encode-batch [[:dom/add-event-listener 2 :click 100]]))
        s (retained/with-draw-ops (reduce retained/apply-op (state) add-ops))
        button-node (some #(when (and (= :node (:draw/op %))
                                      (= :button (:tag %))) %)
                          (:draw-ops s))
        x (+ (:x button-node) 1)
        y (+ (:y button-node) 1)
        events (retained/hit-event s x y :click)]
    (is (= [{:handler 99 :target 2 :name :click :x x :y y}
            {:handler 100 :target 2 :name :click :x x :y y}]
           events))))

(deftest retained-queue-hit-event-enqueues-one-event-per-registered-handler
  ;; queue-hit-event (the fn browser_events.cljs actually calls on a real
  ;; canvas click) must enqueue ALL of them, not just the first -- this is
  ;; the real dispatch path a live pointer event goes through, not just
  ;; the lower-level hit-event helper above.
  (let [add-ops (:ops (abi/encode-batch [[:dom/add-event-listener 2 :click 100]]))
        s (retained/with-draw-ops (reduce retained/apply-op (state) add-ops))
        button-node (some #(when (and (= :node (:draw/op %))
                                      (= :button (:tag %))) %)
                          (:draw-ops s))
        x (+ (:x button-node) 1)
        y (+ (:y button-node) 1)
        s2 (retained/queue-hit-event s x y :click)]
    (is (= [99 100] (mapv :handler (:events s2))))))

(deftest retained-set-text-updates-an-existing-text-nodes-content
  ;; Previously entirely unhandled -- a real Text node's .data/.nodeValue
  ;; setter (or Text.splitText()) emits this op, and without a case here
  ;; the retained-tree host's own copy of that text never changed.
  (let [ops (:ops (abi/encode-batch [[:dom/create-text 50 "old"] [:dom/set-text 50 "new"]]))
        s (reduce retained/apply-op (state) ops)]
    (is (= "new" (get-in s [:nodes 50 :text])))))

(deftest retained-create-fragment-creates-a-document-fragment-node
  (let [ops (:ops (abi/encode-batch [[:dom/create-fragment 60]]))
        s (reduce retained/apply-op (state) ops)]
    (is (= {:node/id 60 :node/type :document-fragment :children []}
           (get-in s [:nodes 60])))))

(deftest retained-focus-then-blur-the-same-node-clears-focus
  (let [ops (:ops (abi/encode-batch [[:dom/focus 2] [:dom/blur 2]]))
        s (reduce retained/apply-op (state) ops)]
    (is (nil? (:focus s)))))

(deftest retained-blur-on-a-different-node-does-not-steal-focus
  ;; Real HTMLElement.blur() only clears focus when the blurring element
  ;; IS the currently-focused one.
  (let [ops (:ops (abi/encode-batch [[:dom/focus 2] [:dom/blur 999]]))
        s (reduce retained/apply-op (state) ops)]
    (is (= 2 (:focus s)))))

(deftest retained-draw-ops-and-hit-test-are-deterministic
  (let [s (retained/with-draw-ops (state))
        button-node (some #(when (and (= :node (:draw/op %))
                                      (= :button (:tag %))) %)
                          (:draw-ops s))
        hit (retained/hit-test s (+ (:x button-node) 1) (+ (:y button-node) 1) :click)]
    (testing "style attrs flow into draw ops"
      (is (= 240 (:w (some #(when (and (= :node (:draw/op %))
                                       (= :main (:tag %))) %)
                           (:draw-ops s)))))
      (is (= "#112233" (:color (some #(when (and (= :rect (:draw/op %))
                                                (= :button (:tag %))) %)
                                    (:draw-ops s))))))
    (testing "hit-test maps pointer coordinates back to the registered handler id(s)"
      (is (= {:target 2 :handlers [99]} hit)))
    (testing "wrong event type does not hit"
      (is (nil? (retained/hit-test s (:x button-node) (:y button-node) :input))))))

(def overlay-ops
  ;; A `position: absolute` overlay (no listener) painted directly over an
  ;; in-flow sibling that DOES have one -- an ordinary modal/dropdown/
  ;; tooltip-over-content layout, not an edge case.
  (:ops
   (abi/encode-batch
    [[:dom/create-element 1 :div]
     [:dom/set-root 1]
     [:dom/set-attr 1 :style/width 200]
     [:dom/create-element 2 :div]
     [:dom/set-attr 2 :style/width 200]
     [:dom/set-attr 2 :style/height 100]
     [:dom/add-event-listener 2 :click 111]
     [:dom/create-element 3 :div]
     [:dom/set-attr 3 :style/position "absolute"]
     [:dom/set-attr 3 :style/left 0]
     [:dom/set-attr 3 :style/top 0]
     [:dom/set-attr 3 :style/width 200]
     [:dom/set-attr 3 :style/height 100]
     [:dom/append-child 1 2]
     [:dom/append-child 1 3]])))

(defn overlay-state []
  (retained/with-draw-ops
    (reduce retained/apply-op (merge retained/base-state {:width 320}) overlay-ops)))

(deftest retained-hit-test-does-not-click-through-a-listener-less-overlay
  ;; Previously reverse-scanned draw-ops (topmost paint order) for the
  ;; first box satisfying BOTH the point-in-box test AND already having a
  ;; matching listener -- skipping straight past node 3 (the listener-
  ;; less absolute overlay, topmost at this point) to node 2 (an unrelated
  ;; sibling underneath that DOES have one), wrongly firing node 2's
  ;; handler. Neither node 3 nor its only real ancestor (the root, node
  ;; 1) has a click listener, so the correct result is no hit at all.
  (is (nil? (retained/hit-test (overlay-state) 50 50 :click))))

(deftest retained-hit-test-bubbles-to-a-real-ancestors-listener
  ;; The fix must not become "an overlay never receives ANY click" --
  ;; when the topmost hit node's OWN ancestor (not an unrelated sibling)
  ;; has the listener, that's a real bubble and must still fire.
  (let [ops (:ops (abi/encode-batch
                   [[:dom/create-element 1 :div]
                    [:dom/set-root 1]
                    [:dom/set-attr 1 :style/width 200]
                    [:dom/add-event-listener 1 :click 222]
                    [:dom/create-element 2 :div]
                    [:dom/set-attr 2 :style/position "absolute"]
                    [:dom/set-attr 2 :style/left 0]
                    [:dom/set-attr 2 :style/top 0]
                    [:dom/set-attr 2 :style/width 100]
                    [:dom/set-attr 2 :style/height 50]
                    [:dom/append-child 1 2]]))
        s (retained/with-draw-ops (reduce retained/apply-op (merge retained/base-state {:width 320}) ops))]
    (is (= {:target 1 :handlers [222]} (retained/hit-test s 10 10 :click)))))

(deftest retained-host-builds-pointer-and-focused-events
  (let [s (retained/with-draw-ops (state))
        button-node (some #(when (and (= :node (:draw/op %))
                                      (= :button (:tag %))) %)
                          (:draw-ops s))
        x (+ (:x button-node) 1)
        y (+ (:y button-node) 1)
        events (retained/hit-event s x y :click)]
    (testing "hit events carry handler, target, event name, and coordinates"
      (is (= [{:handler 99 :target 2 :name :click :x x :y y}] events)))
    (testing "queue-hit-event also establishes focus for keyboard input"
      (let [s (retained/queue-hit-event s x y :click)
            key-events (retained/focused-event s :key-down {:key "Enter"})]
        (is (= 2 (:focus s)))
        (is (= [{:handler 100 :target 2 :name :key-down :key "Enter"}] key-events))))
    (testing "queue-focused-event is a no-op without a focused listener"
      (let [s (retained/queue-focused-event s :input {:value "abc"})]
        (is (empty? (:events s)))))))

;; ---- optional real text-measurement plumbing (retained/draw-ops'
;; measure-text arg -> cssom.layout/draw-ops' :measure-text theme key) ----
;;
;; kotoba.wasm.host.webgl/webgpu's render! already holds a real Canvas 2D
;; `text-ctx` at the exact point it calls retained/draw-ops (used to
;; actually paint text with a real proportional system font), so it can
;; pass a (fn [text font-size font-weight font-style font-family]
;; width-in-px) built from that context's real `measureText` straight
;; through -- see those namespaces'
;; measure-text-fn. This proves that plumbing genuinely reaches
;; cssom.layout's word-wrap (not merely threaded through and dropped),
;; using a fake stand-in for a real Canvas measureText (an honest
;; substitution for the real browser API this JVM test environment has
;; no access to, not a mock of the feature under test).

(def ^:private long-text-ops
  (:ops
   (abi/encode-batch
    [[:dom/create-element 1 :main]
     [:dom/set-root 1]
     [:dom/set-attr 1 :style/width 108]
     [:dom/create-text 2 "WWWWW WWWWW"]
     [:dom/append-child 1 2]])))

(defn- long-text-state []
  (reduce retained/apply-op
          (merge retained/base-state {:width 108})
          long-text-ops))

(defn- fake-proportional-measure
  "A FAKE (fn [text font-size font-weight font-style font-family]
   width-in-px), standing in for a real browser's
   CanvasRenderingContext2D.measureText the same way
   kotoba.wasm.host.webgl/webgpu's measure-text-fn wraps a real one --
   'W' measures much wider per character than the rest, unlike
   cssom.layout's built-in per-character approximation, which cannot
   distinguish characters at all."
  [text _font-size _font-weight _font-style _font-family]
  (reduce + 0 (map (fn [c] (if (= c \W) 16 (if (= c \space) 6 8))) text)))

(deftest retained-draw-ops-without-measure-text-is-unchanged
  ;; No measure-text arg (both the 1-arg call, and with-draw-ops' own
  ;; 1-arg call) -- the exact same output as before this arg existed.
  (let [s (long-text-state)
        ops-1arg (retained/draw-ops s)
        ops-nil (retained/draw-ops s nil)
        text-ops (filterv #(= :text (:draw/op %)) ops-1arg)]
    (is (= ops-1arg ops-nil))
    ;; cssom.layout's default char-w approximation doesn't distinguish
    ;; 'W' from any other character, so this 11-char, 2-word string
    ;; comfortably fits on one line at width 108 (100px content-w).
    (is (= ["WWWWW WWWWW"] (mapv :text text-ops)))))

(deftest retained-draw-ops-with-measure-text-genuinely-changes-wrapping
  (let [s (long-text-state)
        ops (retained/draw-ops s fake-proportional-measure)
        text-ops (filterv #(= :text (:draw/op %)) ops)]
    ;; With the real (fake, in-test) proportional measurement consulted,
    ;; "WWWWW WWWWW" (166px measured) no longer fits the same 100px
    ;; content-w and wraps onto two lines -- a genuinely different wrap
    ;; decision than the unmeasured case above for the exact same input.
    (is (= ["WWWWW" "WWWWW"] (mapv :text text-ops)))))

(deftest with-draw-ops-threads-measure-text-through-too
  (let [s (retained/with-draw-ops (long-text-state) fake-proportional-measure)
        text-ops (filterv #(= :text (:draw/op %)) (:draw-ops s))]
    (is (= ["WWWWW" "WWWWW"] (mapv :text text-ops)))))

;; ---- kotoba.wasm.host.retained/apply-op's :remove-child / :insert-before
;; cases -- before this fix, these ops had no apply-op case and silently fell
;; through to the default `state` branch (a no-op), while abi/op->record
;; would throw first for any caller that goes through the real ABI encode
;; path (host/commit!, abi_runtime_test's host-commit-applies-... test). ----

(defn- retained-state-for [ops]
  (reduce retained/apply-op
          retained/base-state
          (:ops (abi/encode-batch ops))))

(deftest retained-remove-child-detaches-node-from-parents-children
  (let [s (retained-state-for
           [[:dom/create-element 1 :main]
            [:dom/set-root 1]
            [:dom/create-element 2 :span]
            [:dom/create-element 3 :span]
            [:dom/append-child 1 2]
            [:dom/append-child 1 3]
            [:dom/remove-child 1 2]])]
    (testing "the removed child id is gone from the parent's children vector"
      (is (= [3] (get-in s [:nodes 1 :children]))))
    (testing "the detached node is not deleted from the flat node map -- it stays
              as an unreferenced entry, mirroring how :remove-children (plural)
              already leaves its former children in :nodes rather than pruning them"
      (is (= :span (get-in s [:nodes 2 :tag]))))
    (testing "node-tree only walks reachable children, so the detached node no
              longer shows up in the rendered tree even though its :nodes entry survives"
      (is (= [:span] (mapv :tag (:children (retained/node-tree s 1))))))))

(deftest retained-insert-before-places-node-ahead-of-reference-sibling
  (let [s (retained-state-for
           [[:dom/create-element 1 :main]
            [:dom/set-root 1]
            [:dom/create-element 2 :a]
            [:dom/create-element 3 :b]
            [:dom/append-child 1 2]
            [:dom/append-child 1 3]
            [:dom/create-element 4 :c]
            [:dom/insert-before 1 4 3]])]
    (is (= [2 4 3] (get-in s [:nodes 1 :children])))))

(deftest retained-insert-before-falls-back-to-append-without-a-live-reference-sibling
  (testing "a nil before-id appends at the end (mirrors kotoba.wasm.dom/insert-before's own fallback)"
    (let [s (retained-state-for
             [[:dom/create-element 1 :main]
              [:dom/set-root 1]
              [:dom/create-element 2 :a]
              [:dom/append-child 1 2]
              [:dom/create-element 3 :b]
              [:dom/insert-before 1 3 nil]])]
      (is (= [2 3] (get-in s [:nodes 1 :children])))))
  (testing "a before-id that is not (or no longer) a child also falls back to append instead of crashing or silently dropping the insert"
    (let [s (retained-state-for
             [[:dom/create-element 1 :main]
              [:dom/set-root 1]
              [:dom/create-element 2 :a]
              [:dom/append-child 1 2]
              [:dom/create-element 3 :b]
              [:dom/insert-before 1 3 999]])]
      (is (= [2 3] (get-in s [:nodes 1 :children]))))))

(deftest retained-event-queue-is-fifo
  (let [s (-> retained/base-state
              (retained/enqueue-event {:handler 1})
              (retained/enqueue-event {:handler 2}))
        [a s] (retained/poll-event s)
        [b s] (retained/poll-event s)
        [c s] (retained/poll-event s)]
    (is (= {:handler 1} a))
    (is (= {:handler 2} b))
    (is (nil? c))
    (is (empty? (:events s)))))
