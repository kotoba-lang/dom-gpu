(ns kotoba.wasm.host.retained
  "Shared retained-tree host utilities for kotoba:dom renderers.
   WebGL, WebGPU, and native hosts should share this state transition layer so
   ABI semantics are tested once and renderer backends only handle drawing."
  (:require [cssom.layout :as layout]))

(def base-state
  {:nodes {}
   :listeners {}
   :events []
   :draw-ops []})

(defn- normalize-event-name [x]
  (keyword x))

(defn apply-op [state op]
  (case (:op op)
    :create-element
    (assoc-in state [:nodes (:id op)] {:node/id (:id op)
                                       :node/type :element
                                       :tag (keyword (:tag op))
                                       :attrs {}
                                       :children []})

    :create-text
    (assoc-in state [:nodes (:id op)] {:node/id (:id op)
                                       :node/type :text
                                       :text (:text op)})

    :set-root
    (assoc state :root (:id op))

    :set-attr
    (assoc-in state [:nodes (:id op) :attrs (if-let [ns (:namespace op)]
                                               (keyword ns (:name op))
                                               (keyword (:name op)))]
              (:value op))

    ;; Sibling gap to :set-attr above: previously entirely unhandled here
    ;; (falling through to the default `state` below, a silent no-op) --
    ;; a real removeAttribute()/boolean-attribute-off setter never reached
    ;; this state layer at all before, since no op was ever emitted for
    ;; it in the first place. Without this case, the retained tree this
    ;; namespace's own draw-ops/renderers actually paint from would keep
    ;; a removed attribute stale forever.
    :remove-attr
    (update-in state [:nodes (:id op) :attrs] dissoc (if-let [ns (:namespace op)]
                                                        (keyword ns (:name op))
                                                        (keyword (:name op))))

    :append-child
    (update-in state [:nodes (:parent op) :children] (fnil conj []) (:child op))

    :remove-children
    (assoc-in state [:nodes (:id op) :children] [])

    :remove-child
    (update-in state [:nodes (:parent op) :children]
              (fn [children] (vec (remove #{(:child op)} (or children [])))))

    :insert-before
    (update-in state [:nodes (:parent op) :children]
              (fn [children]
                (let [children (vec (or children []))
                      before (:before op)]
                  (if (and before (some #{before} children))
                    (let [[pre post] (split-with #(not= before %) children)]
                      (vec (concat pre [(:child op)] post)))
                    (conj children (:child op))))))

    :add-event-listener
    ;; Real HTML5 addEventListener supports MULTIPLE independent listeners
    ;; for the same (element, event-type) pair -- e.g. a framework and
    ;; user code each attaching their own click handler to the same
    ;; button -- so this keeps an ORDERED collection of handler-ids per
    ;; (node-id, event-name), not a single scalar. Previously a plain
    ;; assoc-in here silently overwrote any prior handler-id for the same
    ;; node+event-type, confirmed via direct REPL reproduction: a second
    ;; add-event-listener op on the same (node, :click) dropped the
    ;; first handler's registration entirely, only the most-recently-
    ;; added one ever matched in hit-test/listener-event below. Mirrors
    ;; kotoba.wasm.dom/add-event-listener's own, already-correct model
    ;; (this retained-tree host layer never got the same fix when that
    ;; one was made) -- adding the exact same handler-id twice is a
    ;; no-op (idempotent), matching real addEventListener semantics for
    ;; a duplicate registration.
    (update-in state [:listeners (:id op) (normalize-event-name (:name op))]
               (fn [ids]
                 (let [ids (or ids [])]
                   (if (some #{(:handler op)} ids) ids (conj ids (:handler op))))))

    :remove-event-listener
    ;; Previously entirely unhandled here (falling through to the default
    ;; `state` below, a silent no-op) -- kotoba.wasm.abi's own encode/
    ;; validate-batch had no case for this op at all either (a real crash,
    ;; "Unknown kotoba DOM op", confirmed via direct REPL reproduction),
    ;; so no real removeEventListener() call had ever reached this far
    ;; before. Without this case, :listeners stayed stale after a real
    ;; removal -- a node's own node-tree projection (and this session's
    ;; own draw-ops :listeners field) would keep reporting a listener
    ;; that real dispatch logic elsewhere had already stopped calling.
    ;;
    ;; Once :add-event-listener above started keeping an ORDERED
    ;; collection of handler-ids (not a scalar), this case's own plain
    ;; `dissoc` of the whole event-type entry became a second, related
    ;; bug: it ignored WHICH handler-id was actually being removed and
    ;; wiped every listener registered for that (node, event-type) pair
    ;; -- confirmed via direct REPL reproduction: removing one of two
    ;; click listeners on the same node dropped BOTH. Mirrors
    ;; kotoba.wasm.dom/remove-event-listener's own, already-correct
    ;; model: remove only the matching handler-id, leaving every other
    ;; listener for that same (element, event-type) pair untouched.
    (let [event-name (normalize-event-name (:name op))
          handler (:handler op)
          remaining (vec (remove #{handler} (get-in state [:listeners (:id op) event-name])))]
      (update-in state [:listeners (:id op)]
                 (fn [by-type]
                   (if (seq remaining)
                     (assoc by-type event-name remaining)
                     (dissoc by-type event-name)))))

    ;; Sibling gap to :remove-event-listener above: kotoba.wasm.abi had no
    ;; case for these four ops either (a real crash, fixed alongside this),
    ;; and this state layer never applied them -- confirmed via direct
    ;; REPL reproduction before touching source.
    :set-text
    (assoc-in state [:nodes (:id op) :text] (:text op))

    :create-fragment
    (assoc-in state [:nodes (:id op)] {:node/id (:id op)
                                       :node/type :document-fragment
                                       :children []})

    :focus
    (assoc state :focus (:id op))

    :blur
    ;; Real HTMLElement.blur() only clears focus when the blurring
    ;; element IS the currently-focused one -- blurring some OTHER,
    ;; not-currently-focused element must not steal focus away from a
    ;; genuinely different, already-focused element.
    (if (= (:focus state) (:id op)) (dissoc state :focus) state)

    state))

(defn node-tree [state id]
  (let [node (get-in state [:nodes id])]
    (case (:node/type node)
      :text (:text node)
      :element (assoc node
                      :listeners (keys (get-in state [:listeners id]))
                      :children (mapv #(node-tree state %) (:children node)))
      node)))

(defn draw-ops
  "Projects `state`'s retained node tree to a flat draw-ops vector (see
   cssom.layout/draw-ops). `measure-text` is an OPTIONAL real
   text-width function (`(fn [text font-size font-weight font-style]
   width-in-px)`) -- e.g. one
   backed by a real browser's `CanvasRenderingContext2D.measureText`,
   which the WebGL/WebGPU hosts already hold as `text-ctx` at the exact
   point they call this fn (see kotoba.wasm.host.webgl/webgpu's render!)
   -- passed straight through to cssom.layout/draw-ops' own optional
   `:measure-text` theme key, so this engine's word-wrap decisions can
   agree with how a real host will actually paint the already-wrapped
   text instead of cssom.layout's built-in per-character approximation.
   Omitting it (every caller before this arg existed, and every host
   without a real measurement function available) leaves cssom.layout's
   default char-w-approximation wrap behavior completely unaffected."
  ([state] (draw-ops state nil))
  ([state measure-text]
   (let [tree (when-let [root (:root state)]
                (node-tree state root))]
     (layout/draw-ops tree (cond-> {:width (:width state)}
                             measure-text (assoc :theme {:measure-text measure-text}))))))

(defn with-draw-ops
  ([state] (with-draw-ops state nil))
  ([state measure-text]
   (assoc state :draw-ops (draw-ops state measure-text))))

(defn enqueue-event [state event]
  (update state :events (fnil conj []) event))

(defn focus [state target]
  (assoc state :focus target))

(defn poll-event [state]
  (let [event (first (:events state))]
    [event (if event (update state :events subvec 1) state)]))

(defn- parent-index
  "Maps every node id to its parent id, derived from each node's own
   `:children` (the only direction `:nodes` stores). Absent from the map
   (nil lookup) means either an untracked id or the real root, which has
   no parent -- both correctly terminate `ancestor-chain` below."
  [state]
  (into {}
        (mapcat (fn [[id node]] (map #(vector % id) (:children node)))
                (:nodes state))))

(defn- ancestor-chain
  "`id` itself, then its parent, grandparent, etc, up to (and including)
   the root -- stops the first time `parents` has no entry."
  [parents id]
  (take-while some? (iterate parents id)))

(defn hit-test
  "Finds the topmost node whose painted box contains (x,y) -- regardless
   of whether IT has a listener -- then walks up its REAL ancestor chain
   for the first one that does, matching real DOM hit-testing + bubbling:
   a listener-less box always blocks (never becomes transparent to)
   whatever happens to be painted underneath it at the same point.

   Previously this reverse-scanned draw-ops (topmost paint order first)
   for the first box satisfying BOTH the point-in-box test AND already
   having a matching listener, skipping straight past any listener-less
   box in between to whatever unrelated node underneath also happened to
   contain the point -- a real click-through: a `position: absolute`
   overlay (a modal, dropdown, tooltip panel -- an ordinary, common
   layout, not an edge case) with no listener of its own, painted over
   an in-flow sibling that DOES have one, let a click on the overlay's
   own blank area silently fire the unrelated sibling underneath's
   handler instead of hitting nothing."
  ;; `:handlers` is a real vector of EVERY handler-id registered for the
  ;; matched (node, event-type) pair, not just one -- matching
  ;; :add-event-listener's own multi-listener storage above. Nothing
  ;; downstream of hit-test's OWN return value currently reads
  ;; `:handlers` for dispatch (hit-event immediately re-queries via
  ;; listener-event below, which is where multi-handler DISPATCH
  ;; actually matters), but this keeps hit-test's own contract honest
  ;; about what's really registered, not silently truncated to one.
  [state x y event-name]
  (let [event-name (normalize-event-name event-name)
        topmost (->> (:draw-ops state)
                     reverse
                     (some (fn [op]
                             (when (and (= :node (:draw/op op))
                                        (<= (:x op) x (+ (:x op) (:w op)))
                                        (<= (:y op) y (+ (:y op) (:h op))))
                               (:id op)))))]
    (when topmost
      (let [parents (parent-index state)]
        (some (fn [id]
                (when-let [handlers (seq (get-in state [:listeners id event-name]))]
                  {:target id :handlers (vec handlers)}))
              (ancestor-chain parents topmost))))))

(defn listener-event
  "Returns a real vector of one event map PER handler-id currently
   registered for (target, event-name), in REGISTRATION ORDER -- matching
   real addEventListener dispatch order and kotoba.wasm.dom/dispatch-event's
   own established convention of one dispatch op per handler invocation.
   Previously returned a single `{:handler ... :target ... :name ...}` map
   assuming a scalar handler, silently dropping every listener after the
   first for a (node, event-type) pair with more than one registered.
   nil (not an empty vector) when there are no listeners at all, so
   callers' existing `(when-let [...] ...)` short-circuit is unchanged."
  ([state target event-name]
   (listener-event state target event-name nil))
  ([state target event-name extra]
   (let [event-name (normalize-event-name event-name)
         handlers (seq (get-in state [:listeners target event-name]))]
     (when handlers
       (mapv (fn [handler]
               (merge {:handler handler
                       :target target
                       :name event-name}
                      extra))
             handlers)))))

(defn hit-event
  "A real vector of event maps (see listener-event), one per handler
   registered on the hit-tested target, or nil if nothing matched."
  ([state x y event-name]
   (hit-event state x y event-name nil))
  ([state x y event-name extra]
   (let [event-name (normalize-event-name event-name)]
     (when-let [{:keys [target]} (hit-test state x y event-name)]
       (listener-event state target event-name (merge {:x x :y y} extra))))))

(defn focused-event
  "A real vector of event maps (see listener-event), one per handler
   registered on the currently-focused node, or nil if none/unfocused."
  ([state event-name]
   (focused-event state event-name nil))
  ([state event-name extra]
   (when-let [target (:focus state)]
     (listener-event state target event-name extra))))

(defn queue-event [state event]
  (cond-> (enqueue-event state event)
    (:target event) (focus (:target event))))

(defn queue-hit-event
  ([state x y event-name]
   (queue-hit-event state x y event-name nil))
  ([state x y event-name extra]
   ;; hit-event/focused-event now return a real vector of one event per
   ;; registered handler (see listener-event), so every matched handler
   ;; gets queued -- previously only the single (post-overwrite) handler
   ;; ever reached the queue at all.
   (if-let [events (hit-event state x y event-name extra)]
     (reduce queue-event state events)
     state)))

(defn queue-focused-event
  ([state event-name]
   (queue-focused-event state event-name nil))
  ([state event-name extra]
   (if-let [events (focused-event state event-name extra)]
     (reduce enqueue-event state events)
     state)))
