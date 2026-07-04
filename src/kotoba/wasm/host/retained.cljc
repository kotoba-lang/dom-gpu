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

    :append-child
    (update-in state [:nodes (:parent op) :children] (fnil conj []) (:child op))

    :remove-children
    (assoc-in state [:nodes (:id op) :children] [])

    :add-event-listener
    (assoc-in state [:listeners (:id op) (normalize-event-name (:name op))] (:handler op))

    state))

(defn node-tree [state id]
  (let [node (get-in state [:nodes id])]
    (case (:node/type node)
      :text (:text node)
      :element (assoc node
                      :listeners (keys (get-in state [:listeners id]))
                      :children (mapv #(node-tree state %) (:children node)))
      node)))

(defn draw-ops [state]
  (let [tree (when-let [root (:root state)]
               (node-tree state root))]
    (layout/draw-ops tree {:width (:width state)})))

(defn with-draw-ops [state]
  (assoc state :draw-ops (draw-ops state)))

(defn enqueue-event [state event]
  (update state :events (fnil conj []) event))

(defn focus [state target]
  (assoc state :focus target))

(defn poll-event [state]
  (let [event (first (:events state))]
    [event (if event (update state :events subvec 1) state)]))

(defn hit-test [state x y event-name]
  (let [event-name (normalize-event-name event-name)]
    (->> (:draw-ops state)
         reverse
         (some (fn [op]
                 (when (and (= :node (:draw/op op))
                            (<= (:x op) x (+ (:x op) (:w op)))
                            (<= (:y op) y (+ (:y op) (:h op)))
                            (get-in state [:listeners (:id op) event-name]))
                   {:target (:id op)
                    :handler (get-in state [:listeners (:id op) event-name])}))))))

(defn listener-event
  ([state target event-name]
   (listener-event state target event-name nil))
  ([state target event-name extra]
   (let [event-name (normalize-event-name event-name)]
     (when-let [handler (get-in state [:listeners target event-name])]
       (merge {:handler handler
               :target target
               :name event-name}
              extra)))))

(defn hit-event
  ([state x y event-name]
   (hit-event state x y event-name nil))
  ([state x y event-name extra]
   (let [event-name (normalize-event-name event-name)]
     (when-let [{:keys [target]} (hit-test state x y event-name)]
       (listener-event state target event-name (merge {:x x :y y} extra))))))

(defn focused-event
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
   (if-let [event (hit-event state x y event-name extra)]
     (queue-event state event)
     state)))

(defn queue-focused-event
  ([state event-name]
   (queue-focused-event state event-name nil))
  ([state event-name extra]
   (if-let [event (focused-event state event-name extra)]
     (enqueue-event state event)
     state)))
