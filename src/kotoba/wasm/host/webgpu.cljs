(ns kotoba.wasm.host.webgpu
  "Browser WebGPU reference host for kotoba:dom.
  It consumes the same ABI as the WebGL host, keeps a retained host tree, emits
  layout draw ops, renders rects with WebGPU, and paints text on a canvas
  overlay until glyph atlas/text shaping is implemented."
  (:require [kotoba.wasm.host.browser-events :as browser-events]
            [kotoba.wasm.host.retained :as retained]))

(def shader-source
  "struct VsOut {
     @builtin(position) pos: vec4<f32>,
     @location(0) color: vec4<f32>,
   };

   @vertex
   fn vs(@location(0) position: vec2<f32>,
         @location(1) color: vec4<f32>) -> VsOut {
     var out: VsOut;
     out.pos = vec4<f32>(position, 0.0, 1.0);
     out.color = color;
     return out;
   }

   @fragment
   fn fs(in: VsOut) -> @location(0) vec4<f32> {
     return in.color;
   }")

(defn- hex->rgba
  "hex is a `#rrggbb` color string, which carries no alpha channel of its
   own -- CSS `opacity` is a separate cascaded value (computed
   cumulatively down the element tree by cssom.layout and stamped onto
   every draw op as `:opacity`, see layout.cljc's `opacity (* opacity
   (:opacity st))`), so callers pass it through explicitly as `alpha`.
   Defaults to fully opaque (1) so every existing call site that doesn't
   pass one is unaffected."
  ([hex] (hex->rgba hex 1))
  ([hex alpha]
   (let [s (if (= \# (first hex)) (subs hex 1) hex)
         n (js/parseInt s 16)]
     [(/ (bit-and (bit-shift-right n 16) 255) 255)
      (/ (bit-and (bit-shift-right n 8) 255) 255)
      (/ (bit-and n 255) 255)
      alpha])))

(defn- resize-canvas! [canvas width height dpr]
  (let [pixel-w (long (* width dpr))
        pixel-h (long (* height dpr))]
    (when (or (not= (.-width canvas) pixel-w)
              (not= (.-height canvas) pixel-h))
      (set! (.-width canvas) pixel-w)
      (set! (.-height canvas) pixel-h))
    (set! (.. canvas -style -width) (str width "px"))
    (set! (.. canvas -style -height) (str height "px"))))

(defn- clip-x [width x]
  (- (* 2 (/ x width)) 1))

(defn- clip-y [height y]
  (- 1 (* 2 (/ y height))))

(defn- rect-vertices [width height {:keys [x y w h color opacity]}]
  (let [[r g b a] (hex->rgba color (or opacity 1))
        x0 (clip-x width x)
        x1 (clip-x width (+ x w))
        y0 (clip-y height y)
        y1 (clip-y height (+ y h))]
    [x0 y0 r g b a
     x1 y0 r g b a
     x0 y1 r g b a
     x0 y1 r g b a
     x1 y0 r g b a
     x1 y1 r g b a]))

(defn- measure-text-fn
  "Builds a (fn [text font-size] width-in-px) backed by `ctx`'s real
   `CanvasRenderingContext2D.measureText` -- see
   kotoba.wasm.host.webgl's identical helper for the full rationale
   (shared here since the WebGPU host paints text through the same
   Canvas 2D text overlay technique, see render-text! below)."
  [ctx]
  (fn [text font-size]
    (set! (.-font ctx) (str font-size "px ui-sans-serif, system-ui, sans-serif"))
    (.-width (.measureText ctx text))))

(defn- render-text! [state ops]
  (let [{:keys [text-ctx text-canvas width height dpr]} state]
    (resize-canvas! text-canvas width height dpr)
    (.save text-ctx)
    (.setTransform text-ctx dpr 0 0 dpr 0 0)
    (.clearRect text-ctx 0 0 width height)
    (doseq [op ops]
      (when (= :text (:draw/op op))
        (set! (.-fillStyle text-ctx) (:color op))
        (set! (.-font text-ctx) (str (:font-size op 14) "px ui-sans-serif, system-ui, sans-serif"))
        (set! (.-globalAlpha text-ctx) (:opacity op 1))
        (.fillText text-ctx (:text op) (:x op) (+ (:y op) (:font-size op 14)))
        (set! (.-globalAlpha text-ctx) 1)))
    (.restore text-ctx)))

(defn- render-gpu! [state ops]
  (let [{:keys [device context pipeline vertex-buffer width height]} state
        ^js device device
        ^js context context
        ^js pipeline pipeline
        ^js vertex-buffer vertex-buffer
        ^js queue (.-queue device)
        rects (filter #(= :rect (:draw/op %)) ops)
        floats (mapcat #(rect-vertices width height %) rects)
        data (js/Float32Array. (clj->js (vec floats)))
        byte-length (.-byteLength data)]
    (when (pos? byte-length)
      (.writeBuffer queue vertex-buffer 0 data))
    (let [^js encoder (.createCommandEncoder device)
          ^js texture (.getCurrentTexture context)
          view (.createView texture)
          ^js pass (.beginRenderPass
                    encoder
                    #js {:colorAttachments
                         #js [#js {:view view
                                   :clearValue #js {:r 0.043 :g 0.055 :b 0.078 :a 1}
                                   :loadOp "clear"
                                   :storeOp "store"}]})]
      (.setPipeline pass pipeline)
      (when (pos? byte-length)
        (.setVertexBuffer pass 0 vertex-buffer)
        (.draw pass (/ (.-length data) 6)))
      (.end pass)
      (.submit queue #js [(.finish encoder)]))))

(defn- render! [state]
  (let [ops (retained/draw-ops state (measure-text-fn (:text-ctx state)))]
    (resize-canvas! (:gpu-canvas state) (:width state) (:height state) (:dpr state))
    (render-gpu! state ops)
    (render-text! state ops)
    (assoc state :draw-ops ops)))

(defn- apply-webgpu-op! [webgpu-host op]
  (swap! (:state webgpu-host) retained/apply-op op)
  nil)

(defn- present-webgpu! [webgpu-host]
  (swap! (:state webgpu-host) render!)
  nil)

(defn- poll-webgpu-event! [webgpu-host]
  (let [[event state] (retained/poll-event @(:state webgpu-host))]
    (when event
      (reset! (:state webgpu-host) state)
      event)))

(defn enqueue-event! [webgpu-host event]
  (swap! (:state webgpu-host) retained/enqueue-event event)
  nil)

(defn install-pointer-events! [webgpu-host canvas pump!]
  (browser-events/install-canvas-events! webgpu-host canvas pump!))

(defn create-host!
  "Returns a Promise resolving to a function-map DomHost."
  [{:keys [gpu-canvas text-canvas width height]}]
  (if-not (.-gpu js/navigator)
    (js/Promise.reject (js/Error. "WebGPU is not available in this browser"))
    (-> (.requestAdapter (.-gpu js/navigator))
        (.then (fn [adapter]
                 (if-not adapter
                   (js/Promise.reject (js/Error. "No WebGPU adapter available"))
                   (let [^js adapter adapter]
                     (.requestDevice adapter)))))
        (.then
         (fn [device]
           (let [^js device device
                 ^js context (.getContext gpu-canvas "webgpu")
                 format (.getPreferredCanvasFormat (.-gpu js/navigator))
                 text-ctx (.getContext text-canvas "2d")
                 module (.createShaderModule device #js {:code shader-source})
                 pipeline (.createRenderPipeline
                           device
                           #js {:layout "auto"
                                :vertex #js {:module module
                                             :entryPoint "vs"
                                             :buffers
                                             #js [#js {:arrayStride 24
                                                       :attributes
                                                       #js [#js {:shaderLocation 0 :offset 0 :format "float32x2"}
                                                            #js {:shaderLocation 1 :offset 8 :format "float32x4"}]}]}
                                :fragment #js {:module module
                                               :entryPoint "fs"
                                               ;; Standard "over" alpha blending so :rect ops
                                               ;; carrying a fractional :opacity (nested CSS
                                               ;; `opacity` cascaded down by cssom.layout, threaded
                                               ;; through rect-vertices' per-vertex color above)
                                               ;; actually blend against whatever this render pass
                                               ;; already drew/cleared instead of just overwriting
                                               ;; it -- without this, the fragment shader's alpha
                                               ;; output has no visual effect at all.
                                               :targets #js [#js {:format format
                                                                  :blend #js {:color #js {:srcFactor "src-alpha"
                                                                                          :dstFactor "one-minus-src-alpha"
                                                                                          :operation "add"}
                                                                              :alpha #js {:srcFactor "src-alpha"
                                                                                          :dstFactor "one-minus-src-alpha"
                                                                                          :operation "add"}}}]}
                                :primitive #js {:topology "triangle-list"}})
                 vertex-buffer (.createBuffer device #js {:size (* 6 4096 4)
                                                          :usage (bit-or js/GPUBufferUsage.VERTEX
                                                                         js/GPUBufferUsage.COPY_DST)})]
             ;; alphaMode only governs how this canvas's own final output
             ;; alpha composites against the DOM page behind it (parity
             ;; with webgl.cljs's `#js {:alpha false}` context attribute);
             ;; it has no effect on how draw ops within a single frame
             ;; blend against each other or against the pass's own
             ;; clearValue -- that is entirely the render pipeline's
             ;; :blend state above. So "opaque" stays correct here and
             ;; does not need to become "premultiplied" for this fix.
             (.configure context #js {:device device :format format :alphaMode "opaque"})
             {:state (atom (merge retained/base-state
                                  {:device device
                                   :context context
                                   :pipeline pipeline
                                   :vertex-buffer vertex-buffer
                                   :gpu-canvas gpu-canvas
                                   :text-canvas text-canvas
                                   :text-ctx text-ctx
                                   :width (or width 640)
                                   :height (or height 360)
                                   :dpr (or (.-devicePixelRatio js/window) 1)}))
              :apply-op! apply-webgpu-op!
              :present! present-webgpu!
              :poll-event! poll-webgpu-event!}))))))

(defn state [webgpu-host]
  @(:state webgpu-host))
