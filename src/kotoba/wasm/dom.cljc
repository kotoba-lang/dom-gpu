(ns kotoba.wasm.dom
  "DOM-like substrate for kotoba WASM.
   This does not call browser DOM APIs. It maintains a document model and emits
   host-friendly ops that a WebGL/WebGPU/native host can interpret."
  (:require [clojure.string :as str]))

(def empty-document
  {:next-id 1
   :root nil
   :nodes {}
   :listeners {}
   :ops []})

(defn- alloc-id [document]
  [(:next-id document) (update document :next-id inc)])

(defn- emit [document op]
  (update document :ops conj op))

(defn consume-ops [document]
  [(:ops document) (assoc document :ops [])])

(defn create-element [document tag]
  (let [[id document] (alloc-id document)
        tag (keyword tag)]
    [id (-> document
            (assoc-in [:nodes id] {:node/id id
                                   :node/type :element
                                   :tag tag
                                   :attrs {}
                                   :children []})
            (emit [:dom/create-element id tag]))]))

(defn create-text-node [document text]
  (let [[id document] (alloc-id document)
        text (str text)]
    [id (-> document
            (assoc-in [:nodes id] {:node/id id
                                   :node/type :text
                                   :text text})
            (emit [:dom/create-text id text]))]))

(defn set-root [document node-id]
  (-> document
      (assoc :root node-id)
      (emit [:dom/set-root node-id])))

(defn set-attribute [document node-id k v]
  (-> document
      (assoc-in [:nodes node-id :attrs (keyword k)] v)
      (emit [:dom/set-attr node-id (keyword k) v])))

(defn set-style [document node-id style]
  (reduce-kv
   (fn [document k v]
     (set-attribute document node-id (keyword "style" (name k)) v))
   document
   style))

(defn append-child [document parent-id child-id]
  (-> document
      (update-in [:nodes parent-id :children] (fnil conj []) child-id)
      (emit [:dom/append-child parent-id child-id])))

(defn insert-before [document parent-id child-id before-id]
  (-> document
      (update-in [:nodes parent-id :children]
                 (fn [children]
                   (let [children (vec (or children []))]
                     (if (and before-id (some #{before-id} children))
                       (let [[before after] (split-with #(not= before-id %) children)]
                         (vec (concat before [child-id] after)))
                       (conj children child-id)))))
      (emit [:dom/insert-before parent-id child-id before-id])))

(defn remove-child [document parent-id child-id]
  (-> document
      (update-in [:nodes parent-id :children]
                 (fn [children] (vec (remove #{child-id} (or children [])))))
      (emit [:dom/remove-child parent-id child-id])))

(defn remove-children [document node-id]
  (-> document
      (assoc-in [:nodes node-id :children] [])
      (emit [:dom/remove-children node-id])))

(defn add-event-listener [document node-id event-name handler-id]
  (-> document
      (assoc-in [:listeners node-id (keyword event-name)] handler-id)
      (emit [:dom/add-event-listener node-id (keyword event-name) handler-id])))

(defn dispatch-event [document node-id event-name event]
  (if-let [handler-id (get-in document [:listeners node-id (keyword event-name)])]
    (emit document [:dom/dispatch-event handler-id event])
    document))

(defn node [document node-id]
  (get-in document [:nodes node-id]))

(defn tree [document]
  (letfn [(walk [id]
            (let [n (node document id)]
              (case (:node/type n)
                :text (:text n)
                :element (assoc n
                                :listeners (keys (get-in document [:listeners id]))
                                :children (mapv walk (:children n)))
                n)))]
    (some-> (:root document) walk)))

(defn text-content [document]
  (letfn [(walk [id]
            (let [n (node document id)]
              (case (:node/type n)
                :text (:text n)
                :element (str/join "" (map walk (:children n)))
                "")))]
    (if-let [root (:root document)] (walk root) "")))
