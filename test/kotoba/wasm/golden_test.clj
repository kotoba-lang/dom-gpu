(ns kotoba.wasm.golden-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [kotoba.wasm.abi :as abi]
            [kotoba.wasm.host.retained :as retained]))

(def source-ops
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
   [:dom/append-child 1 2]])

(defn read-resource [path]
  (edn/read-string (slurp (io/resource path))))

(defn retained-state []
  (reduce retained/apply-op
          (merge retained/base-state {:width 320})
          (:ops (abi/encode-batch source-ops))))

(deftest abi-batch-golden-remains-stable
  (is (= (read-resource "kotoba/wasm/golden/retained_batch.edn")
         (abi/encode-batch source-ops))))

(deftest retained-draw-ops-golden-remains-stable
  ;; The `<button>`'s `:font-size` in this golden moved 13 -> 13.3333 on
  ;; 2026-08-05, and it is the only value in it that did. It is not a
  ;; rendering change here: cssom.layout's UA control font was always the
  ;; browser's 13.3333px Arial and was TRUNCATED to 13 on purpose, because
  ;; that truncation cancelled an opposite error in the intrinsic WIDTH
  ;; model (see its own ua-control-font). Both halves moved together once
  ;; the missing metric -- the font's average character advance -- turned
  ;; out to be measurable; the width half arrives here through the new
  ;; `:avg-advance`/`:max-advance` hooks the WebGL and WebGPU hosts now
  ;; supply. This golden is built with no host hooks at all, so it sees
  ;; only the size.
  (is (= (read-resource "kotoba/wasm/golden/retained_draw_ops.edn")
         (:draw-ops (retained/with-draw-ops (retained-state))))))

(deftest retained-input-events-golden-remain-stable
  (let [s (retained/with-draw-ops (retained-state))
        button-node (some #(when (and (= :node (:draw/op %))
                                      (= :button (:tag %))) %)
                          (:draw-ops s))
        x (+ (:x button-node) 1)
        y (+ (:y button-node) 1)
        s (-> s
              (retained/queue-hit-event x y :click)
              (retained/queue-focused-event :key-down {:key "Enter"}))]
    (is (= (read-resource "kotoba/wasm/golden/retained_events.edn")
           (:events s)))))
