(ns vlojure.graphics
  (:require ["pixi.js" :as pixi]
            ["fontfaceobserver" :as FaceFontObserver]
            [vlojure.util :as u]
            [vlojure.constants :as c]
            [vlojure.geometry :refer [add-points
                                      subtract-points
                                      scale-point
                                      angle-point
                                      point-magnitude
                                      rect-around
                                      unit-square
                                      PI]]
            [vlojure.storage :refer [color-scheme]]
            [vlojure.quil :refer [quil-mode?
                                  quil-width
                                  init-quil
                                  resize-quil]]
            [clojure.string :refer [index-of split]]
            [clojure.set :refer [union]]
            ["canvg" :refer [Canvg] :rename {Canvg canvg}]))

(defonce pixi-app (atom nil))
(defonce pixi-graphics (atom {}))
(defonce text-containers (atom {}))
(defonce svg-image-containers (atom {}))
(defonce form-icon-image-containers (atom {}))
(defonce current-svg-color-scheme (atom nil))
(defonce font-loaded? (atom false))
(defonce layer-texts (atom {}))
(defonce layer-used-text-counts (atom {}))
(defonce svg-textures (atom {}))
(defonce svg-sprites (atom {}))
(defonce form-icon-sprites (atom {}))
(defonce svg-sprite-active-counts (atom {}))
(defonce requested-svg-texture-resolutions (atom {}))
(defonce form-icon-sprite-active-counts (atom {}))
(defonce form-icon-container (atom nil))
(defonce form-icon-graphics (atom nil))
(defonce form-icon-size (atom 0))
(defonce form-icon-textures (atom {}))
(defonce form-icon-texture-sizes (atom {}))
(defonce form-renderer-busy? (atom false))

(defn get-graphics [& [layer]]
  (if (= layer :form-icon)
    @form-icon-graphics
    (get @pixi-graphics
         (or layer (first c/ui-layers)))))

(defn html-color [color]
  (str "#"
       (apply str
              (mapv (fn [index]
                      (let [num (mod (int
                                      (/ color
                                         (Math/pow 16 index)))
                                     16)]
                        (or ({10 "A"
                              11 "B"
                              12 "C"
                              13 "D"
                              14 "E"
                              15 "F"} num)
                            num)))
                    (reverse (range 6))))))

(defn app-width []
  (if (quil-mode?)
    (- (.-innerWidth js/window) (quil-width))
    (.-innerWidth js/window)))
(defn app-height [] (.-innerHeight js/window))
(defn app-size [] (min (app-width) (app-height)))
(defn layer-size [layer]
  (if (= layer :form-icon) @form-icon-size (app-size)))
(defn app-aspect-ratio [] (/ (app-width) (app-height)))
(defn app-rect []
  (rect-around unit-square
               (app-aspect-ratio)))

(defn screen-x [x & [layer]]
  (if (= layer :form-icon)
    (* x @form-icon-size)
    (let [w (app-width)
          h (app-height)
          s (min w h)]
      (+ (* 0.5 (- w s)) (* x s)))))

(defn screen-y [y & [layer]]
  (if (= layer :form-icon)
    (* y @form-icon-size)
    (let [w (app-width)
          h (app-height)
          s (min w h)]
      (+ (* 0.5 (- h s)) (* y s)))))

(defn text-size [s & [layer]]
  (* (layer-size layer)
     (min c/text-max-size
          (/ c/text-scale-factor
             (apply max (map count (split s "\n")))))))

(defn resize []
  (.resize (.-renderer @pixi-app)
           (app-width)
           (app-height))
  (resize-quil (app-width) (app-height)))

(defn draw-rect [[pos size] fill & [layer]]
  (let [graphics (get-graphics layer)]
    (.beginFill graphics fill)
    (.drawRect graphics
               (screen-x (:x pos) layer)
               (screen-y (:y pos) layer)
               (* (:x size) (layer-size layer))
               (* (:y size) (layer-size layer)))
    (.endFill graphics)))

(defn draw-circle [{:keys [x y radius]} fill & [layer]]
  (let [graphics (get-graphics layer)]
    (.beginFill graphics fill)
    (.drawCircle graphics
                 (screen-x x layer)
                 (screen-y y layer)
                 (* radius (layer-size layer)))
    (.endFill graphics)))

(defn draw-polygon [points fill & [layer]]
  (let [graphics (get-graphics layer)]
    (.beginFill graphics fill)
    (.drawPolygon graphics
                  (clj->js (mapv #(pixi/Point. (screen-x (:x %) layer)
                                               (screen-y (:y %) layer))
                                 points)))
    (.endFill graphics)))

(defn draw-line [start end width color & [layer]]
  (let [graphics (get-graphics layer)]
    (.lineStyle graphics
                (* width (layer-size layer))
                color)
    (.moveTo graphics
             (screen-x (:x start) layer)
             (screen-y (:y start) layer))
    (.lineTo graphics
             (screen-x (:x end) layer)
             (screen-y (:y end) layer))
    (.lineStyle graphics 0)))

(defn draw-polyline [points width color & [layer]]
  (let [graphics (get-graphics layer)]
    (.lineStyle graphics
                (* width (layer-size layer))
                color)
    (let [start (first points)]
      (.moveTo graphics
               (screen-x (:x start) layer)
               (screen-y (:y start) layer)))
    (doseq [point (rest points)]
      (.lineTo graphics
               (screen-x (:x point) layer)
               (screen-y (:y point) layer)))
    (.lineStyle graphics 0)))

(defn free-used-texts! []
  (doseq [layer (keys @layer-used-text-counts)]
    (swap! layer-used-text-counts
           #(assoc % layer 0))
    (doseq [[layer texts] @layer-texts]
      (when (not= layer :form-icon)
        (doseq [t texts]
          (set! (.-visible t) false))))))

(defn clear-form-icon-canvas! []
  (.clear @form-icon-graphics)
  (let [texts (:form-icon @layer-texts)]
    (doseq [t texts]
      (set! (.-visible t) false))))

(defn after-render [callback]
  (let [ticker (.-ticker @pixi-app)
        delay (atom 1)
        ticker-fn (fn ticker-fn []
                    (if (zero? @delay)
                      (do (.remove ticker ticker-fn)
                          (callback))
                      (swap! delay dec)))]
    (.add ticker ticker-fn)))

(defn take-form-icon-renderer! []
  (reset! form-renderer-busy? true))

(defn free-form-icon-renderer! []
  (reset! form-renderer-busy? false))

(defn is-form-icon-renderer-busy? []
  @form-renderer-busy?)

(defn get-unused-text! [layer]
  (let [text-vector (or (get @layer-texts layer) [])
        text-count (or (get @layer-used-text-counts layer) 0)]
    (swap! layer-used-text-counts
           #(update % layer inc))
    (if (< text-count (count text-vector))
      (text-vector text-count)
      (let [t (pixi/BitmapText. ""
                                (clj->js {:fontName c/font-name
                                          :fontSize 10
                                          :align "left"}))]
        (swap! layer-texts
               #(assoc % layer (conj text-vector t)))
        (.addChild (get @text-containers layer) t)
        t))))

(defn draw-text [s pos size color & [layer]]
  (when @font-loaded?
    (let [t (get-unused-text! layer)
          scale (* size (text-size s layer))]
      (set! (.-text t) (str s))
      (set! (.-tint t) color)
      (set! (.-visible t) true)
      (set! (.-scale.x t) scale)
      (set! (.-scale.y t) scale)
      (set! (.-x t)
            (+ (- (screen-x (:x pos) layer)
                  (* (.-width t) 0.5))
               (* scale c/text-x-offset)))
      (set! (.-y t)
            (+ (- (screen-y (:y pos) layer)
                  (* (.-height t) 0.5))
               (* scale c/text-y-offset))))))

(defn get-delta []
  (/ (.-elapsedMS (.-ticker @pixi-app)) 1000))

(defn update-svg-color-scheme [color-scheme]
  (when (not= color-scheme @current-svg-color-scheme)
    (doseq [class [:background :foreground :highlight :text]]
      (let [elements (.getElementsByClassName js/document (subs (str class) 1))
            color (color-scheme class)
            rgb-string (str "#"
                            (apply str
                                   (map #(.toString
                                          (mod (quot color
                                                     (Math/pow 256 %))
                                               256)
                                          16)
                                        (reverse (range 3)))))]
        (doseq [i (range (.-length elements))]
          (let [element (.item elements i)]
            (doseq [attribute ["fill" "stroke"]]
              (let [current-value (.getAttribute element attribute)]
                (when (and current-value
                           (not= current-value "none"))
                  (.setAttribute element attribute rgb-string))))))))
    (reset! current-svg-color-scheme color-scheme)))

(defn svgs-ready? []
  (boolean
   (js/document.getElementById "undo")))

(defn blob->img [blob]
  (let [url (.createObjectURL js/URL blob)
        img (js/Image.)]
    (.addEventListener img
                       "load"
                       (fn [event]
                         (.revokeObjectURL js/URL
                                           url)))
    (set! (.-src img) url)
    img))

(defn create-svg-texture! [svg-name resolution]
  (let [existing-requested-resolution
        (@requested-svg-texture-resolutions svg-name)]
    (when (< existing-requested-resolution resolution)
      (swap! requested-svg-texture-resolutions
             assoc
             svg-name
             resolution)
      (swap! svg-sprites
             #(dissoc % svg-name))
      (swap! svg-sprite-active-counts
             #(dissoc % svg-name))
      (update-svg-color-scheme (color-scheme))
      (let [canvas (js/document.createElement "canvas")
            context (.getContext canvas "2d")
            svg (js/document.getElementById svg-name)]
        (.setAttribute svg "width" (str resolution "px"))
        (.setAttribute svg "height" (str resolution "px"))
        (.then (.from canvg context (.-outerHTML svg))
               (fn [canvg-result]
                 (.then
                  (.render canvg-result)
                  (fn [& _]
                    (.toBlob canvas
                             (fn [blob]
                               (let [texture (pixi/Texture.
                                              (pixi/BaseTexture.
                                               (blob->img blob)))]
                                 (swap! svg-textures
                                        #(assoc % svg-name texture)))))))))))))

(defn clear-svg-textures! []
  (reset! svg-textures {}))

(defn get-svg-sprite [svg-name]
  (when (not (get @svg-sprites svg-name))
    (swap! svg-sprites
           #(assoc % svg-name []))
    (swap! svg-sprite-active-counts
           #(assoc % svg-name 0)))
  (let [texture (get @svg-textures svg-name)]
    (when texture
      (let [existing-sprites (get @svg-sprites svg-name)
            active-count (get @svg-sprite-active-counts svg-name)]
        (swap! svg-sprite-active-counts
               #(update % svg-name inc))
        (if (< active-count (count existing-sprites))
          (existing-sprites active-count)
          (let [new-sprite (pixi/Sprite. texture)]
            (swap! svg-sprites
                   #(update % svg-name conj new-sprite))
            new-sprite))))))

(defn free-used-svg-images! []
  (swap! svg-sprite-active-counts
         #(reduce (fn [active-counts key]
                    (assoc active-counts key 0))
                  %
                  (keys %)))
  (doseq [[_ container] @svg-image-containers]
    (while (pos? (.-children.length container))
      (let [child (.getChildAt container 0)]
        (.removeChild container child)))))

(defn get-form-icon-sprite [form]
  (when (not (get @form-icon-sprites form))
    (swap! form-icon-sprites
           #(assoc % form []))
    (swap! form-icon-sprite-active-counts
           #(assoc % form 0)))
  (let [texture (get @form-icon-textures form)]
    (when texture
      (let [existing-sprites (get @form-icon-sprites form)
            active-count (get @form-icon-sprite-active-counts form)]
        (swap! form-icon-sprite-active-counts
               #(update % form inc))
        (if (< active-count (count existing-sprites))
          (existing-sprites active-count)
          (let [new-sprite (pixi/Sprite. texture)]
            (swap! form-icon-sprites
                   #(update % form conj new-sprite))
            new-sprite))))))

(defn free-used-form-icon-images! []
  (swap! form-icon-sprite-active-counts
         #(reduce (fn [active-counts key]
                    (assoc active-counts key 0))
                  %
                  (keys %)))
  (doseq [[_ container] @form-icon-image-containers]
    (while (pos? (.-children.length container))
      (let [child (.getChildAt container 0)]
        (.removeChild container child)))))

(defn icon-texture-size [form]
  (@form-icon-texture-sizes form))

(defn draw-form-icon [form {:keys [x y radius]} layer]
  (let [size (* 2 radius
                c/form-icon-canvas-overflow-factor
                (/ (.-width (@form-icon-textures form))
                   (icon-texture-size form)))
        sprite (get-form-icon-sprite form)
        sprite-size (* (layer-size layer)
                       size)]
    (set! (.-width sprite) sprite-size)
    (set! (.-height sprite) sprite-size)
    (set! (.-x sprite) (screen-x (- x (* 0.5 size))))
    (set! (.-y sprite) (screen-y (- y (* 0.5 size))))
    (.addChild (get @form-icon-image-containers layer) sprite)))

(defn draw-svg [name {:keys [x y]} size layer]
  (let [name (if (keyword? name)
               (subs (str name) 1)
               name)
        svg-size (* (app-size) size)]
    (when (svgs-ready?)
      (let [existing-texture (get @svg-textures name)
            int-svg-size (Math/ceil svg-size)]
        (when (or (not existing-texture)
                  (and (> (.-width existing-texture) 1)
                       (> int-svg-size (.-width existing-texture))))
          (create-svg-texture! name int-svg-size)))
      (let [sprite (get-svg-sprite name)]
        (when sprite
          (set! (.-width sprite) svg-size)
          (set! (.-height sprite) svg-size)
          (set! (.-x sprite) (screen-x (- x (* 0.5 size))))
          (set! (.-y sprite) (screen-y (- y (* 0.5 size))))
          (.addChild (get @svg-image-containers layer) sprite))))))

(defn render-tool [tool-name
                   {:keys [radius] :as tool-circle}
                   layer
                   & [outline?]]
  (draw-svg tool-name
            tool-circle
            (* 2 radius)
            layer)
  (when outline?
    (draw-circle (update tool-circle
                         :radius
                         (partial * (inc c/formbar-outline-thickness)))
                 (:foreground (color-scheme))
                 layer)
    (draw-circle tool-circle
                 (:background (color-scheme))
                 layer)))

(defn update-graphics []
  (resize)
  (doseq [layer c/ui-layers]
    (.clear (get-graphics layer)))
  (free-used-texts!)
  (free-used-svg-images!)
  (free-used-form-icon-images!))

(defn load-font []
  (let [font (FaceFontObserver. c/font-name)]
    (.then (.load font nil 500)
           (fn []
             (reset! font-loaded? true)
             (.from pixi/BitmapFont c/font-name
                    (clj->js
                     {:fontFamily c/font-name
                      :fill 0xffffff
                      :fontSize 300})
                    (clj->js
                     {:chars pixi/BitmapFont.ASCII}))
             (u/log "Font loaded."))
           load-font)))

(defn resize-form-renderer [size]
  (reset! form-icon-size size))

(defn create-form-icon-image! [form finish-callback]
  (js/setTimeout (fn []
                   (set! (.-width @form-icon-container)
                         @form-icon-size)
                   (set! (.-height @form-icon-container)
                         @form-icon-size)
                   (let [renderer (.-renderer @pixi-app)
                         texture (pixi/Texture.
                                  (.generateTexture renderer
                                                    @form-icon-container))]
                     (swap! form-icon-textures
                            assoc
                            form
                            texture)
                     (swap! form-icon-texture-sizes
                            assoc
                            form
                            @form-icon-size))
                   (swap! form-icon-sprites
                          dissoc
                          form)
                   (free-form-icon-renderer!)
                   (finish-callback))
                 0))

(defn init [update-fn click-down-fn click-up-fn update-mouse-fn]
  (reset! pixi-app
          (pixi/Application. (clj->js {:autoResize true})))
  (reset! form-icon-container
          (pixi/Container.))
  (set! (.-zIndex @form-icon-container)
        ##-Inf)
  (reset! form-icon-graphics
          (pixi/Graphics.))
  (.addChild @form-icon-container
             @form-icon-graphics)
  (.addChild (.-stage @pixi-app)
             @form-icon-container)
  (let [form-icon-text-container (pixi/Container.)]
    (.addChild @form-icon-container
               form-icon-text-container)
    (swap! text-containers
           assoc
           :form-icon
           form-icon-text-container))
  (let [stage (.-stage @pixi-app)]
    (set! (.-sortableChildren stage) true)
    (doseq [[layer z] (mapv vector c/ui-layers (range))]
      (let [graphics (pixi/Graphics.)]
        (set! (.-zIndex graphics) z)
        (.addChild stage graphics)
        (swap! pixi-graphics
               #(assoc % layer graphics)))
      (let [image-container (pixi/Container.)]
        (set! (.-zIndex image-container) (+ z 0.25))
        (.addChild stage image-container)
        (swap! form-icon-image-containers
               #(assoc % layer image-container)))
      (let [image-container (pixi/Container.)]
        (set! (.-zIndex image-container) (+ z 0.5))
        (.addChild stage image-container)
        (swap! svg-image-containers
               #(assoc % layer image-container)))
      (let [text-container (pixi/Container.)]
        (set! (.-zIndex text-container) (+ z 0.75))
        (.addChild stage text-container)
        (swap! text-containers
               #(assoc % layer text-container))))
    (js/document.body.appendChild (.-view @pixi-app))
    (.add (.-ticker @pixi-app) update-fn)
    (let [interaction (get (js->clj (.-plugins (.-renderer @pixi-app)))
                           "interaction")]
      (.on interaction "pointerdown" click-down-fn)
      (.on interaction "pointerup" click-up-fn)
      (.on interaction "pointermove" update-mouse-fn))
    (load-font)
    (resize))
  (init-quil))

(defn in-discard-corner? [pos]
  (let [[app-pos app-size] (app-rect)]
    (<= (point-magnitude
         (subtract-points (add-points app-pos
                                      (select-keys app-size [:y]))
                          pos))
        c/lower-corner-zone-radius)))

(defn render-discard-zone [& [highlighted? blank-symbol?]]
  (let [[app-pos app-size] (app-rect)]
    (draw-circle (assoc (add-points app-pos
                                    (select-keys app-size [:y]))
                        :radius c/lower-corner-zone-radius)
                 (if highlighted?
                   (:highlight (color-scheme))
                   (:foreground (color-scheme)))
                 :menu)
    (draw-circle (assoc (add-points app-pos
                                    (select-keys app-size [:y]))
                        :radius (* (- 1 c/corner-zone-bar-thickness)
                                   c/lower-corner-zone-radius))
                 (:background (color-scheme))
                 :menu)
    (when blank-symbol?
      (let [radius (/ (* (- 1 c/corner-zone-bar-thickness)
                         c/lower-corner-zone-radius)
                      (inc (Math/sqrt 2)))
            base-circle-pos (-> app-pos
                                (update :y (partial + (- (:y app-size)
                                                         radius)))
                                (update :x (partial + radius)))
            angle-offset (scale-point (angle-point (* 0.25 PI))
                                      (* radius
                                         c/discard-zone-icon-radius-factor))]
        (draw-circle (assoc base-circle-pos
                            :radius (* radius
                                       c/discard-zone-icon-radius-factor))
                     (:foreground (color-scheme))
                     :menu)
        (draw-circle (assoc base-circle-pos
                            :radius (* radius
                                       c/discard-zone-icon-radius-factor
                                       (- 1 c/discard-zone-icon-thickness)))
                     (:background (color-scheme))
                     :menu)
        (draw-line (add-points base-circle-pos
                               angle-offset)
                   (subtract-points base-circle-pos
                                    angle-offset)
                   (* radius
                      (* c/discard-zone-icon-radius-factor
                         c/discard-zone-icon-thickness))
                   (:foreground (color-scheme))
                   :menu)))))

(defn clear-form-icons! []
 (doseq [form-icon-atom [form-icon-textures
                         form-icon-texture-sizes
                         form-icon-sprites]]
   (reset! form-icon-atom
           {})))
