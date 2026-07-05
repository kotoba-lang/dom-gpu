(ns kotoba.wasm.host.webgl
  "Browser reference host for kotoba:dom.
   UI is not DOM widgets: the host realizes kotoba DOM ops into retained nodes,
   projects them to draw ops, draws rectangles with WebGL, and paints text on a
  canvas overlay."
  (:require [kotoba.wasm.host :as host]
            [kotoba.wasm.host.browser-events :as browser-events]
            [kotoba.wasm.host.retained :as retained]))

(def vertex-source
  "attribute vec2 a_pos;
   uniform vec2 u_resolution;
   void main() {
     vec2 zero_to_one = a_pos / u_resolution;
     vec2 clip = zero_to_one * 2.0 - 1.0;
     gl_Position = vec4(clip * vec2(1.0, -1.0), 0.0, 1.0);
   }")

(def fragment-source
  "precision mediump float;
   uniform vec4 u_color;
   void main() {
     gl_FragColor = u_color;
   }")

(defn- shader! [gl type source]
  (let [s (.createShader gl type)]
    (.shaderSource gl s source)
    (.compileShader gl s)
    (when-not (.getShaderParameter gl s (.-COMPILE_STATUS gl))
      (throw (js/Error. (.getShaderInfoLog gl s))))
    s))

(defn- program! [gl]
  (let [p (.createProgram gl)]
    (.attachShader gl p (shader! gl (.-VERTEX_SHADER gl) vertex-source))
    (.attachShader gl p (shader! gl (.-FRAGMENT_SHADER gl) fragment-source))
    (.linkProgram gl p)
    (when-not (.getProgramParameter gl p (.-LINK_STATUS gl))
      (throw (js/Error. (.getProgramInfoLog gl p))))
    p))

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

(defn- measure-text-fn
  "Builds a (fn [text font-size] width-in-px) backed by `ctx`'s real
   `CanvasRenderingContext2D.measureText` -- passed down into
   cssom.layout/draw-ops (via retained/draw-ops's own optional
   measure-text arg) so word-wrap decisions agree with the real,
   proportional system font `render!` actually paints text with below,
   instead of cssom.layout's built-in per-character monospace-like
   approximation. Sets `ctx`'s `.font` to match `font-size` before each
   measurement, the same font string render!'s own :text paint case
   already uses, so a measured width always corresponds to the font this
   host is really about to paint the text in."
  [ctx]
  (fn [text font-size]
    (set! (.-font ctx) (str font-size "px ui-sans-serif, system-ui, sans-serif"))
    (.-width (.measureText ctx text))))

(defn- draw-rect! [gl buffer position-loc color-loc x y w h color opacity]
  (let [[r g b a] (hex->rgba color opacity)
        verts (js/Float32Array.
               #js [x y
                    (+ x w) y
                    x (+ y h)
                    x (+ y h)
                    (+ x w) y
                    (+ x w) (+ y h)])]
    (.bindBuffer gl (.-ARRAY_BUFFER gl) buffer)
    (.bufferData gl (.-ARRAY_BUFFER gl) verts (.-STATIC_DRAW gl))
    (.enableVertexAttribArray gl position-loc)
    (.vertexAttribPointer gl position-loc 2 (.-FLOAT gl) false 0 0)
    (.uniform4f gl color-loc r g b a)
    (.drawArrays gl (.-TRIANGLES gl) 0 6)))

(defn- render! [state]
  (let [{:keys [gl text-ctx gl-canvas text-canvas program buffer width height dpr]} state
        ops (retained/draw-ops state (measure-text-fn text-ctx))
        position-loc (.getAttribLocation gl program "a_pos")
        resolution-loc (.getUniformLocation gl program "u_resolution")
        color-loc (.getUniformLocation gl program "u_color")]
    (resize-canvas! gl-canvas width height dpr)
    (resize-canvas! text-canvas width height dpr)
    (.viewport gl 0 0 (.-width gl-canvas) (.-height gl-canvas))
    (.useProgram gl program)
    (.uniform2f gl resolution-loc width height)
    (.clearColor gl 0.043 0.055 0.078 1)
    (.clear gl (.-COLOR_BUFFER_BIT gl))
    (.save text-ctx)
    (.setTransform text-ctx dpr 0 0 dpr 0 0)
    (.clearRect text-ctx 0 0 width height)
    (doseq [op ops]
      (case (:draw/op op)
        :rect (draw-rect! gl buffer position-loc color-loc
                          (:x op) (:y op) (:w op) (:h op) (:color op) (:opacity op 1))
        :text (do
                (set! (.-fillStyle text-ctx) (:color op))
                (set! (.-font text-ctx) (str (:font-size op 14) "px ui-sans-serif, system-ui, sans-serif"))
                (set! (.-globalAlpha text-ctx) (:opacity op 1))
                (.fillText text-ctx (:text op) (:x op) (+ (:y op) (:font-size op 14)))
                (set! (.-globalAlpha text-ctx) 1))
        :node nil
        nil))
    (.restore text-ctx)
    (assoc state :draw-ops ops)))

(defn- apply-webgl-op! [webgl-host op]
  (swap! (:state webgl-host) retained/apply-op op)
  nil)

(defn- present-webgl! [webgl-host]
  (swap! (:state webgl-host) render!)
  nil)

(defn- poll-webgl-event! [webgl-host]
  (let [[event state] (retained/poll-event @(:state webgl-host))]
    (when event
      (reset! (:state webgl-host) state)
      event)))

(defn enqueue-event! [webgl-host event]
  (swap! (:state webgl-host) retained/enqueue-event event)
  nil)

(defn install-pointer-events! [webgl-host canvas pump!]
  (browser-events/install-canvas-events! webgl-host canvas pump!))

(defn create-host! [{:keys [gl-canvas text-canvas width height]}]
  (let [gl (.getContext gl-canvas "webgl" #js {:alpha false :antialias true})
        text-ctx (.getContext text-canvas "2d")
        program (program! gl)
        buffer (.createBuffer gl)]
    ;; Standard "over" alpha blending so :rect ops carrying a fractional
    ;; :opacity (nested CSS `opacity` cascaded down by cssom.layout)
    ;; actually blend against whatever is already in the color buffer
    ;; (previously drawn rects, or the clearColor) instead of just
    ;; overwriting it -- without this, u_color's alpha channel has no
    ;; visual effect at all regardless of what draw-rect! passes it.
    (.enable gl (.-BLEND gl))
    (.blendFunc gl (.-SRC_ALPHA gl) (.-ONE_MINUS_SRC_ALPHA gl))
    {:state (atom (merge retained/base-state
                         {:gl gl
                          :text-ctx text-ctx
                          :gl-canvas gl-canvas
                          :text-canvas text-canvas
                          :program program
                          :buffer buffer
                          :width (or width 640)
                          :height (or height 360)
                          :dpr (or (.-devicePixelRatio js/window) 1)}))
     :apply-op! apply-webgl-op!
     :present! present-webgl!
     :poll-event! poll-webgl-event!}))

(defn state [webgl-host]
  @(:state webgl-host))
