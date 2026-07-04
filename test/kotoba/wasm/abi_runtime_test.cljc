(ns kotoba.wasm.abi-runtime-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.wasm.abi :as abi]
            [kotoba.wasm.dom :as dom]
            [kotoba.wasm.host :as host]
            [cssom.layout :as layout]
            [kotoba.wasm.runtime :as runtime]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(defn reset-counter! [n]
  (rf/clear!)
  (rf/reg-event-db :init (fn [_ _] {:count n :events []}))
  (rf/reg-event-db :inc (fn [db [_ event]]
                          (-> db
                              (update :count inc)
                              (update :events conj event))))
  (rf/reg-sub :count (fn [db _] (:count db)))
  (rf/reg-sub :events (fn [db _] (:events db)))
  (rf/dispatch-sync [:init]))

(defn reset-input-capture! []
  (rf/clear!)
  (rf/reg-event-db :init-input (fn [_ _] {:value "" :events []}))
  (rf/reg-event-db :capture-input
                   (fn [db [_ event]]
                     (-> db
                         (assoc :value (:value event))
                         (update :events conj event))))
  (rf/reg-event-db :capture-key
                   (fn [db [_ event]]
                     (update db :events conj event)))
  (rf/reg-sub :input-value (fn [db _] (:value db)))
  (rf/reg-sub :events (fn [db _] (:events db)))
  (rf/dispatch-sync [:init-input]))

(defn rich-counter []
  [:main#root.shell
   [:h1 "Counter"]
   (for [label ["a" "b"]]
     [:span.item label])
   nil
   false
   [:button#inc.primary.large
    {:class [:hot "wide"]
     :style {:padding "12px" :width "120px" :background "#334455"}
     :on-click (fn [event] (rf/dispatch [:inc event]))}
    "inc"]
   [:span.value @(rf/subscribe [:count])]])

(defn input-capture []
  [:main
   [:input#name
    {:value @(rf/subscribe [:input-value])
     :on-input (fn [event] (rf/dispatch [:capture-input event]))
     :on-key-down (fn [event] (rf/dispatch [:capture-key event]))}]
   [:span.value @(rf/subscribe [:input-value])]])

(deftest reagent-compatible-shorthand-and-seq-children
  (reset-counter! 2)
  (let [{:keys [document handlers]} (r/render [rich-counter])
        root (dom/node document (:root document))
        button-id (->> (:nodes document)
                       (some (fn [[id node]]
                               (when (= "inc" (get-in node [:attrs :id])) id))))
        button (dom/node document button-id)]
    (testing "tag shorthand maps to tag/id/class attrs"
      (is (= :main (:tag root)))
      (is (= "root" (get-in root [:attrs :id])))
      (is (= "shell" (get-in root [:attrs :class]))))
    (testing "class attrs merge with shorthand classes"
      (is (= "primary large hot wide" (get-in button [:attrs :class]))))
    (testing "style map becomes namespaced style attributes for host ABI"
      (is (= "12px" (get-in button [:attrs :style/padding])))
      (is (= "120px" (get-in button [:attrs :style/width]))))
    (testing "lazy child seqs are flattened and nil/false children are ignored"
      (is (= "Counterabinc2" (dom/text-content document))))
    (is (= 1 (count handlers)))))

(deftest abi-batch-validation-covers-invalid-inputs
  (is (= {:abi/version 1
          :ops [{:op :create-element :id 1 :tag "div"}
                {:op :set-attr :id 1 :namespace "style" :name "width" :value "12"}]}
         (abi/encode-batch [[:dom/create-element 1 :div]
                            [:dom/set-attr 1 :style/width 12]])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported"
                        (abi/validate-batch {:abi/version 999 :ops []})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid create-element"
                        (abi/validate-batch {:abi/version 1
                                             :ops [{:op :create-element :id "bad" :tag "div"}]})))
  (doseq [[message op] [["Invalid create-element" [:dom/create-element 1 nil]]
                        ["Invalid set-attr" [:dom/set-attr 1 nil "x"]]
                        ["Invalid add-event-listener" [:dom/add-event-listener 1 nil 9]]
                        ["Invalid add-event-listener" [:dom/add-event-listener 1 :click nil]]
                        ["Invalid append-child" [:dom/append-child 1 nil]]
                        ["Invalid remove-children" [:dom/remove-children nil]]]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo (re-pattern message)
                          (abi/validate-batch (abi/encode-batch [op])))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown"
                        (abi/encode-batch [[:dom/nope 1]]))))

(deftest dom-document-ops-cover-removal-dispatch-and-consume
  (let [[root-id document] (dom/create-element dom/empty-document :main)
        [child-id document] (dom/create-text-node document "x")
        document (-> document
                     (dom/set-root root-id)
                     (dom/append-child root-id child-id)
                     (dom/add-event-listener root-id :click 42))
        document (dom/dispatch-event document root-id :click {:x 1})
        [ops document] (dom/consume-ops document)
        document (dom/remove-children document root-id)]
    (is (= "x" (dom/text-content (assoc document :root root-id :nodes (assoc-in (:nodes document) [root-id :children] [child-id])))))
    (is (some #(= [:dom/dispatch-event 42 {:x 1}] %) ops))
    (is (= [[:dom/remove-children root-id]] (:ops document)))
    (is (= "" (dom/text-content document)))
    (is (= [[:dom/remove-children root-id]] (:ops document)))))

(deftest function-map-host-commits-through-same-runtime-path
  (let [state (atom {:ops [] :events [] :present-count 0})
        function-host {:apply-op! (fn [_ op] (swap! state update :ops conj op))
                       :present! (fn [_] (swap! state update :present-count inc))
                       :poll-event! (fn [_] nil)}
        batch (host/commit! function-host [[:dom/create-element 1 :button]
                                           [:dom/set-root 1]])]
    (is (= 1 (:present-count @state)))
    (is (= [:create-element :set-root] (mapv :op (:ops @state))))
    (is (= (:ops batch) (:ops @state)))))

(deftest host-commit-validates-before-mutating-host
  (let [state (atom {:ops [] :present-count 0})
        function-host {:apply-op! (fn [_ op] (swap! state update :ops conj op))
                       :present! (fn [_] (swap! state update :present-count inc))
                       :poll-event! (fn [_] nil)}]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid add-event-listener"
                          (host/commit! function-host [[:dom/create-element 1 :button]
                                                       [:dom/add-event-listener 1 nil 9]])))
    (is (= [] (:ops @state)))
    (is (zero? (:present-count @state)))))

(deftest runtime-event-normalization-and-unknown-handler-behavior
  (reset-counter! 0)
  (let [mounted (runtime/mount! (host/recording-host) rich-counter)
        handler-id (first (keys (:handlers mounted)))
        dom-host (host/recording-host [{:handler handler-id
                                        :target 4
                                        :name :click
                                        :x 10
                                        :y 20
                                        :value 9}
                                       {:handler 999 :target 4 :name :click}])
        mounted (runtime/mount! dom-host rich-counter)
        pumped (runtime/pump-events! mounted)]
    (is (= "Counterabinc1" (dom/text-content (:document pumped))))
    (is (= [{:target 4 :name "click" :x 10.0 :y 20.0 :value "9" :key nil}]
           @(rf/subscribe [:events])))))

(deftest runtime-pumps-input-and-key-events-from-reagent-listeners
  (reset-input-capture!)
  (let [mounted (runtime/mount! (host/recording-host) input-capture)
        input-id (->> (get-in mounted [:document :nodes])
                      (some (fn [[id node]]
                              (when (= "name" (get-in node [:attrs :id])) id))))
        input-handler (get-in mounted [:document :listeners input-id :input])
        key-handler (get-in mounted [:document :listeners input-id :key-down])
        dom-host (host/recording-host [{:handler input-handler
                                        :target input-id
                                        :name :input
                                        :value "kotoba"}
                                       {:handler key-handler
                                        :target input-id
                                        :name :key-down
                                        :key :Enter}])
        mounted (runtime/mount! dom-host input-capture)
        pumped (runtime/pump-events! mounted)]
    (is (= "kotoba" (dom/text-content (:document pumped))))
    (is (= [{:target input-id :name "input" :x 0.0 :y 0.0 :value "kotoba" :key nil}
            {:target input-id :name "key-down" :x 0.0 :y 0.0 :value nil :key "Enter"}]
           @(rf/subscribe [:events])))))

(deftest layout-respects-basic-style-and-hit-semantics
  (reset-counter! 5)
  (let [tree (dom/tree (:document (r/render [rich-counter])))
        ops (layout/draw-ops tree {:width 480})
        button-rect (some #(when (and (= :rect (:draw/op %)) (= :button (:tag %))) %) ops)
        button-node (some #(when (and (= :node (:draw/op %)) (= :button (:tag %))) %) ops)]
    (is (= "#334455" (:color button-rect)))
    (is (= 120 (:w button-node)))
    (is (= "primary large hot wide" (:class button-node)))
    (is (= [:click] (:listeners button-node)))))
