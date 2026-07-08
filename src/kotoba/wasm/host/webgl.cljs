(ns kotoba.wasm.host.webgl
  "Browser reference host for kotoba:dom.
   UI is not DOM widgets: the host realizes kotoba DOM ops into retained nodes,
   projects them to draw ops, draws rectangles with WebGL, and paints text on a
  canvas overlay."
  (:require [kotoba.wasm.host :as host]
            [kotoba.wasm.host.browser-events :as browser-events]
            [kotoba.wasm.host.color :as color]
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

(defn- resize-canvas! [canvas width height dpr]
  (let [pixel-w (long (* width dpr))
        pixel-h (long (* height dpr))]
    (when (or (not= (.-width canvas) pixel-w)
              (not= (.-height canvas) pixel-h))
      (set! (.-width canvas) pixel-w)
      (set! (.-height canvas) pixel-h))
    (set! (.. canvas -style -width) (str width "px"))
    (set! (.. canvas -style -height) (str height "px"))))

(defn- text-font-string
  "The real Canvas 2D `font` CSS-like string for a `:text` draw-op,
   interpolating `:font-weight`/`:font-style` (cssom.layout now threads
   both through onto every :text op) ahead of the font-size/family that
   were always here -- before this, a real, cascade-computed CSS
   `font-weight: bold`/`font-style: italic` had ZERO visual effect,
   confirmed via direct REPL reproduction that the resolved :style/
   font-weight/:style/font-style attrs existed but layout's own :text
   draw-op never carried them at all. Absent or literally `\"normal\"`
   (real CSS's own initial value for both properties) contributes
   nothing to the string, matching this op's exact pre-existing
   behavior byte-for-byte when neither property was ever set. A numeric
   font-weight (`\"700\"`) is passed through verbatim rather than
   restricted to the `bold`/`normal` keywords -- Canvas 2D's own `font`
   property accepts a numeric weight directly, same as real CSS.

   `:font-family`, likewise now threaded through by cssom.layout, is
   interpolated ahead of the SAME fixed `ui-sans-serif, system-ui,
   sans-serif` fallback this always used -- a real author `font-family`
   had ZERO visual effect no matter what a real page declared, every
   :text draw-op hardcoding this same fallback regardless of the real,
   cascade-computed `:style/font-family` attr, the exact same bug shape
   already fixed for font-weight/font-style above. Passed through
   VERBATIM (not validated against a known-font allowlist) -- a real
   browser's own Canvas 2D `font` string accepts any author-supplied
   family list unchanged too, falling back to a generic family (this
   engine's own fixed fallback) only when the whole list fails to
   resolve to an installed font, exactly matching real CSS font-stack
   fallback behavior.

   Also reused by measure-text-fn below (given an equivalent map shape)
   so word-wrap MEASUREMENT and real PAINT always agree on which exact
   font string a run of text uses."
  [op]
  (str (when-let [fw (:font-weight op)] (when (not= "normal" fw) (str fw " ")))
       (when-let [fs (:font-style op)] (when (not= "normal" fs) (str fs " ")))
       (:font-size op 14) "px " (or (:font-family op) "ui-sans-serif, system-ui, sans-serif")))

(defn- measure-text-fn
  "Builds a (fn [text font-size font-weight font-style font-family]
   width-in-px) backed by `ctx`'s real
   `CanvasRenderingContext2D.measureText` -- passed down into
   cssom.layout/draw-ops (via retained/draw-ops's own optional
   measure-text arg) so word-wrap decisions agree with the real,
   proportional system font `render!` actually paints text with
   below, instead of cssom.layout's built-in per-character monospace-
   like approximation. Sets `ctx`'s `.font` via text-font-string (the
   exact same helper render!'s own :text paint case uses) before each
   measurement, so a measured width always corresponds to the REAL font
   -- including real bold/italic metrics -- this host is really about
   to paint the text in.

   Closes a real, previously-documented gap: cssom.layout's OLDER
   `(fn [text font-size] ...)` callback contract had no font-weight/
   font-style parameter at all, so word-wrap measurement for bold/
   italic text always used NORMAL-weight, upright metrics even though
   the PAINT step already correctly rendered bold/italic -- a real, if
   minor and rarely visible, inconsistency (bold text is typically
   ~5-10% wider than normal at the same point size) between where a
   line wraps and how wide its real glyphs render, confirmed via direct
   REPL reproduction (a long bold string wrapped identically to the
   same string in normal weight) before this fix. cssom.layout's own
   callback contract was extended to pass font-weight/font-style
   through; this is the real host-side half of that same change. Now
   also takes a 5th `font-family` arg for the exact same reason -- an
   author's real font-family can genuinely have different, wider/
   narrower per-character metrics than the fixed system-font fallback
   word-wrap measurement always assumed before, another instance of the
   same measurement/paint disagreement class as font-weight/font-style
   above."
  [ctx]
  (fn [text font-size font-weight font-style font-family]
    (set! (.-font ctx) (text-font-string {:font-size font-size :font-weight font-weight :font-style font-style
                                          :font-family font-family}))
    (.-width (.measureText ctx text))))

(defn- rect-intersect
  "Intersects two `{:x :y :w :h}` rects (same top-left, logical-pixel
   shape :clip/:rect/:text draw ops already share). Used to narrow a
   nested `:clip` push down to whatever clip region was already active,
   per cssom.layout/layout-block's :clip push/pop pairs, which can nest
   (a scrollable container inside another scrollable container) -- each
   nested push must intersect with its ancestor's clip, not replace it."
  [a b]
  (let [x1 (max (:x a) (:x b))
        y1 (max (:y a) (:y b))
        x2 (min (+ (:x a) (:w a)) (+ (:x b) (:w b)))
        y2 (min (+ (:y a) (:h a)) (+ (:y b) (:h b)))]
    {:x x1 :y y1 :w (max 0 (- x2 x1)) :h (max 0 (- y2 y1))}))

(defn- clip-rect-px
  "Converts a `:clip` draw op's x/y/w/h -- CSS/logical pixels, the same
   coordinate space :rect/:text draw ops already use -- into physical
   framebuffer pixels by scaling by `dpr`, mirroring resize-canvas!'s own
   `(long (* n dpr))` scaling of the canvas element itself. `gl.scissor`
   operates on the physical framebuffer, not the logical CSS pixel grid
   draw ops are expressed in, so it needs this same scaling `draw-rect!`'s
   vertex-shader normalization gets for free via u_resolution."
  [dpr op]
  {:x (long (* dpr (:x op))) :y (long (* dpr (:y op)))
   :w (long (* dpr (:w op))) :h (long (* dpr (:h op)))})

(defn- scissor!
  "WebGL's scissor box origin is bottom-left, unlike the top-left
   draw-ops coordinate system -- flip y using the physical canvas height,
   the same top-left/bottom-left reconciliation the vertex shader already
   does for vertex positions via `clip * vec2(1.0, -1.0)` in
   vertex-source above."
  [gl canvas-h {:keys [x y w h]}]
  (.scissor gl x (- canvas-h (+ y h)) w h))

(defn- draw-text-decoration!
  "Paints `:text-decoration`'s `underline`/`overline`/`line-through` as a
   thin filled rect on `text-ctx`, spanning the real, measured width of
   `(:text op)` at the CURRENT `text-ctx.font` (called after `.fillText`
   below, so bold/italic's actual wider/narrower glyphs are measured, not
   normal-weight metrics). cssom.layout now threads a resolved
   `:text-decoration` onto every :text draw-op the exact same way it
   already threads `:font-weight`/`:font-style` (see text-font-string) --
   before this, a real, cascade-resolved `text-decoration: underline` had
   ZERO visual effect, confirmed via direct REPL reproduction that the
   resolved :style/text-decoration attr existed but no draw-op ever
   carried it through to a real paint call. `line-through`/`overline`'s
   vertical offsets from `baseline` are simple, approximate fractions of
   `font-size` -- this engine has no real font-metrics/ascent/descent of
   its own, matching the existing char-width approximation layout.cljc's
   own word-wrap already uses -- not exact glyph-metrics placement, but
   visually correct and distinguishable for all three real CSS keywords.
   Absent, `\"none\"`, or any other unrecognized value paints nothing,
   matching this op's exact pre-existing (no decoration at all) behavior
   byte-for-byte. Deliberately scoped to a single keyword at a time --
   real CSS's own `text-decoration-line: underline overline` multi-value
   form is not supported."
  [text-ctx op baseline]
  (when-let [decoration (:text-decoration op)]
    (let [font-size (:font-size op 14)
          line-y (case decoration
                   "underline" (+ baseline (* font-size 0.12))
                   "line-through" (- baseline (* font-size 0.3))
                   "overline" (- baseline (* font-size 0.9))
                   nil)]
      (when line-y
        (let [text-width (.-width (.measureText text-ctx (:text op)))
              thickness (max 1 (/ font-size 12))]
          (.fillRect text-ctx (:x op) line-y text-width thickness))))))

(defn- draw-rect! [gl buffer position-loc color-loc x y w h color opacity]
  (let [[r g b a] (color/->rgba color opacity)
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
        ;; (max height ...), never a bare replacement: a caller-supplied
        ;; :height stays an honored MINIMUM (e.g. "always show at least a
        ;; full viewport of background even for a short/empty page"), the
        ;; content's own real extent only ever grows the canvas, never
        ;; shrinks it below that floor. See retained/content-height's own
        ;; docstring for the bug this guards against and why it now lives
        ;; in the shared kotoba.wasm.host.retained namespace instead of
        ;; being duplicated per-backend (webgpu.cljs's render! shares it).
        height (max height (retained/content-height ops))
        position-loc (.getAttribLocation gl program "a_pos")
        resolution-loc (.getUniformLocation gl program "u_resolution")
        color-loc (.getUniformLocation gl program "u_color")]
    (resize-canvas! gl-canvas width height dpr)
    (resize-canvas! text-canvas width height dpr)
    (.viewport gl 0 0 (.-width gl-canvas) (.-height gl-canvas))
    (.useProgram gl program)
    (.uniform2f gl resolution-loc width height)
    ;; Defensively reset scissor state before this frame's own clear/draws
    ;; -- if a previous frame's ops somehow left the clip stack unbalanced
    ;; (SCISSOR_TEST would otherwise carry over and clip the .clear below).
    (.disable gl (.-SCISSOR_TEST gl))
    (.clearColor gl 0.043 0.055 0.078 1)
    (.clear gl (.-COLOR_BUFFER_BIT gl))
    (.save text-ctx)
    (.setTransform text-ctx dpr 0 0 dpr 0 0)
    (.clearRect text-ctx 0 0 width height)
    ;; A plain doseq can't thread the nested `:clip` stack (see
    ;; rect-intersect) through to later ops, so this loop carries it as
    ;; `stack` -- a vector of already-intersected `{:x :y :w :h}` rects,
    ;; one per currently-open :clip push, narrowest (most recent) last.
    (loop [remaining ops stack []]
      (when-let [op (first remaining)]
        (recur
         (rest remaining)
         (case (:draw/op op)
           :rect (do (draw-rect! gl buffer position-loc color-loc
                                 (:x op) (:y op) (:w op) (:h op) (:color op) (:opacity op 1))
                     stack)
           :text (let [baseline (+ (:y op) (:font-size op 14))]
                   (set! (.-fillStyle text-ctx) (:color op))
                   (set! (.-font text-ctx) (text-font-string op))
                   (set! (.-globalAlpha text-ctx) (:opacity op 1))
                   (.fillText text-ctx (:text op) (:x op) baseline)
                   (draw-text-decoration! text-ctx op baseline)
                   (set! (.-globalAlpha text-ctx) 1)
                   stack)
           :clip (case (:clip/op op)
                   :push
                   (let [canvas-w (.-width gl-canvas)
                         canvas-h (.-height gl-canvas)
                         prev (or (peek stack) {:x 0 :y 0 :w canvas-w :h canvas-h})
                         rect (rect-intersect prev (clip-rect-px dpr op))]
                     (.enable gl (.-SCISSOR_TEST gl))
                     (scissor! gl canvas-h rect)
                     ;; Canvas2D's own .clip() intersects with whatever
                     ;; clip path is already active at the time of the
                     ;; matching .save() (per spec), so nesting needs no
                     ;; manual rect-intersection bookkeeping the way
                     ;; gl.scissor's single active rect does above.
                     (.save text-ctx)
                     (.beginPath text-ctx)
                     (.rect text-ctx (:x op) (:y op) (:w op) (:h op))
                     (.clip text-ctx)
                     (conj stack rect))
                   :pop
                   (let [stack' (pop stack)]
                     (if (seq stack')
                       (scissor! gl (.-height gl-canvas) (peek stack'))
                       (.disable gl (.-SCISSOR_TEST gl)))
                     (.restore text-ctx)
                     stack'))
           :node stack
           stack))))
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
