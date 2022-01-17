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
(defonce svg-queue (atom ()))
(defonce current-svg-color-scheme (atom nil))
(defonce font-loaded? (atom false))
(defonce layer-texts (atom {}))
(defonce layer-used-text-counts (atom {}))
(defonce svg-textures (atom {}))

(defn get-graphics [& [layer]]
  (get @pixi-graphics
       (or layer (first c/ui-layers))))

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
(defn app-aspect-ratio [] (/ (app-width) (app-height)))
(defn app-rect []
  (rect-around unit-square
               (app-aspect-ratio)))

(defn screen-x [x]
  (let [w (app-width)
        h (app-height)
        s (min w h)]
    (+ (* 0.5 (- w s)) (* x s))))

(defn screen-y [y]
  (let [w (app-width)
        h (app-height)
        s (min w h)]
    (+ (* 0.5 (- h s)) (* y s))))

(defn get-mouse-pos []
  (let [plugins (.-plugins (.-renderer @pixi-app))
        raw-pos (clj->js (.-global (.-mouse (get (js->clj plugins) "interaction"))))
        x (.-x raw-pos)
        y (.-y raw-pos)
        width (app-width)
        height (app-height)
        size (app-size)]
    {:x (/ (- x (* 0.5 (- width size))) size)
     :y (/ (- y (* 0.5 (- height size))) size)}))

(defn text-size [s]
  (* (app-size)
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
               (screen-x (:x pos))
               (screen-y (:y pos))
               (* (:x size) (app-size))
               (* (:y size) (app-size)))
    (.endFill graphics)))

(defn draw-circle [{:keys [x y radius]} fill & [layer]]
  (let [graphics (get-graphics layer)]
    (.beginFill graphics fill)
    (.drawCircle graphics
                 (screen-x x)
                 (screen-y y)
                 (* radius (app-size)))
    (.endFill graphics)))

(defn draw-polygon [points fill & [layer]]
  (let [graphics (get-graphics layer)]
    (.beginFill graphics fill)
    (.drawPolygon graphics
                  (clj->js (mapv #(pixi/Point. (screen-x (:x %))
                                               (screen-y (:y %)))
                                 points)))
    (.endFill graphics)))

(defn draw-line [start end width color & [layer]]
  (let [graphics (get-graphics layer)]
    (.lineStyle graphics
                (* width (app-size))
                color)
    (.moveTo graphics
             (screen-x (:x start))
             (screen-y (:y start)))
    (.lineTo graphics
             (screen-x (:x end))
             (screen-y (:y end)))
    (.lineStyle graphics 0)))

(defn draw-polyline [points width color & [layer]]
  (let [graphics (get-graphics layer)]
    (.lineStyle graphics
                (* width (app-size))
                color)
    (let [start (first points)]
      (.moveTo graphics
               (screen-x (:x start))
               (screen-y (:y start))))
    (doseq [point (rest points)]
      (.lineTo graphics
               (screen-x (:x point))
               (screen-y (:y point))))
    (.lineStyle graphics 0)))

(defn free-used-texts! []
  (doseq [layer (keys @layer-used-text-counts)]
    (swap! layer-used-text-counts
           #(assoc % layer 0))
    (doseq [kv-pair @layer-texts]
      (doseq [t (second kv-pair)]
        (set! (.-visible t) false)))))

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
          scale (* size (text-size s))]
      (set! (.-text t) (str s))
      (set! (.-tint t) color)
      (set! (.-visible t) true)
      (set! (.-scale.x t) scale)
      (set! (.-scale.y t) scale)
      (set! (.-x t)
            (+ (- (screen-x (:x pos))
                  (* (.-width t) 0.5))
               (* scale c/text-x-offset)))
      (set! (.-y t)
            (+ (- (screen-y (:y pos))
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
                                   (map #(.toString (mod (quot color
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

(defn create-svg-texture! [svg-name resolution]
  (update-svg-color-scheme (color-scheme))
  (let [canvas (js/document.createElement "canvas")
        context (.getContext canvas "2d")
        svg (js/document.getElementById svg-name)]
    (.setAttribute svg "width" (str resolution "px"))
    (.setAttribute svg "height" (str resolution "px"))
    (let [blob->img
          (fn [blob]
            (let [url (.createObjectURL js/URL blob)
                  img (js/Image.)]
              (.addEventListener img
                                 "load"
                                 (fn [event]
                                   (.revokeObjectURL js/URL
                                                     url)))
              (set! (.-src img) url)
              (let [texture (pixi/Texture. (pixi/BaseTexture. img))]
                (swap! svg-textures
                       #(assoc % svg-name texture)))))]
      (.then (.from canvg context (.-outerHTML svg))
             (fn [canvg-result]
               (.then (.render canvg-result)
                      (fn [& _]
                        (.toBlob canvas
                                 blob->img))))))))

(defn clear-svg-textures! []
  (reset! svg-textures {}))

(defn get-svg-sprite [svg-name]
  (let [texture (get @svg-textures svg-name)]
    (when texture
      (pixi/Sprite. texture))))

(defn free-used-svg-images! []
  (doseq [[_ container] @svg-image-containers]
    (while (pos? (.-children.length container))
      (let [child (.getChildAt container 0)]
        (.removeChild container child)))))

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

(defn new-svg-copy [name]
  (let [name-str (cond
                   (keyword? name) (subs (str name) 1)
                   :else (str name))
        element (.cloneNode (js/document.getElementById name-str) true)]
    (js/document.body.appendChild element)
    (.remove (.-classList element) "base-svg")
    (.add (.-classList element) "copy-svg")
    (set! (.-id element) (str (.-id element) "_"))
    element))

(defn update-svgs []
  (let [required-svgs-by-name (reduce (fn [element-map svg-args]
                                        (let [[name pos radius] svg-args]
                                          (update element-map
                                                  name
                                                  #(conj % [pos radius]))))
                                      {}
                                      @svg-queue)
        copy-svgs (js/document.getElementsByClassName "copy-svg")
        existing-svgs-by-name (reduce (fn [element-map svg]
                                        (let [id (.-id svg)
                                              name (keyword (subs id
                                                                  0
                                                                  (or (index-of id "_")
                                                                      (count id))))]
                                          (update element-map
                                                  name
                                                  #(conj % svg))))
                                      {}
                                      (mapv #(.item copy-svgs %)
                                            (range (.-length copy-svgs))))
        names (union (set (keys required-svgs-by-name))
                     (set (keys existing-svgs-by-name)))]
    (doseq [name names]
      (let [required-svgs (vec (name required-svgs-by-name))
            existing-svgs (vec (name existing-svgs-by-name))
            max-svg-count (max (count required-svgs)
                               (count existing-svgs))]
        (loop [current-svgs existing-svgs
               index 0]
          (when (< index max-svg-count)
            (let [expanded-svgs (if (< index
                                       (count current-svgs))
                                  current-svgs
                                  (conj current-svgs
                                        (new-svg-copy name)))]
              (recur (update expanded-svgs
                             index
                             (fn [svg]
                               (set! (.-id svg)
                                     (str (subs
                                           (str name)
                                           1)
                                          "_"
                                          index))
                               (if (< index (count required-svgs))
                                 (do (set! (.-visibility (.-style svg))
                                           "visible")
                                     (let [[{:keys [x y]} radius] (required-svgs index)
                                           width (int (.getAttribute svg "width"))
                                           height (int (.getAttribute svg "height"))
                                           pixel-x (- (screen-x x) (* width 0.5))
                                           pixel-y (- (screen-y y) (* height 0.5))]
                                       (set! (.-left (.-style svg)) (str pixel-x "px"))
                                       (set! (.-top (.-style svg)) (str pixel-y "px"))
                                       (.setAttribute svg
                                                      "transform"
                                                      (str "scale("
                                                           (/ (* 2 radius (app-size))
                                                              (max width height))
                                                           ")")
                                                      "scale()")))
                                 (set! (.-visibility (.-style svg))
                                       "hidden"))

                               svg))
                     (inc index))))))))
  (reset! svg-queue ()))

(defn update-graphics []
  (resize)
  (doseq [layer c/ui-layers]
    (.clear (get-graphics layer)))
  (free-used-texts!)
  (free-used-svg-images!))

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

(defn init [update-fn click-down-fn click-up-fn update-mouse-fn]
  (reset! pixi-app
          (pixi/Application. (clj->js {:autoResize true})))
  (let [stage (.-stage @pixi-app)]
    (set! (.-sortableChildren stage) true)
    (doseq [[layer z] (mapv vector c/ui-layers (range))]
      (let [graphics (pixi/Graphics.)]
        (set! (.-zIndex graphics) z)
        (.addChild stage graphics)
        (swap! pixi-graphics
               #(assoc % layer graphics)))
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
    (let [interaction (get (js->clj (.-plugins (.-renderer @pixi-app))) "interaction")]
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
                                (update :y (partial + (- (:y app-size) radius)))
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
