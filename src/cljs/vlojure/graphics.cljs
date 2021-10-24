(ns vlojure.graphics
  (:require ["pixi.js" :as pixi]
            ["fontfaceobserver" :as FaceFontObserver]
            [vlojure.util :as u]
            [vlojure.geometry :as geom]
            [vlojure.constants :as constants]
            [vlojure.storage :as storage]))

(defonce graphics-state (atom {}))

(defn get-graphics [& [layer]]
  (get (:graphics @graphics-state)
       (or layer (first constants/ui-layers))))

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

(defn attr [key]
  (get @graphics-state key))

(defn set-attr! [key value]
  (swap! graphics-state
         #(assoc % key value))
  value)

(defn update-attr! [key value]
  (swap! graphics-state
         #(update % key value)))

(defn app-width [] (.-innerWidth js/window))
(defn app-height [] (.-innerHeight js/window))
(defn app-size [] (min (app-width) (app-height)))
(defn app-aspect-ratio [] (/ (app-width) (app-height)))
(defn app-rect []
  (geom/rect-around geom/unit-square
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
  (let [plugins (.-plugins (.-renderer (:app @graphics-state)))
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
     (min constants/text-max-size
          (/ constants/text-scale-factor
             (count s)))))

(defn resize []
  (let [current-width (app-width)
        current-height (app-height)]
    (.resize (.-renderer (:app @graphics-state)) current-width current-height)))

(defn rect [[pos size] fill & [layer]]
  (let [graphics (get-graphics layer)]
    (.beginFill graphics fill)
    (.drawRect graphics
               (screen-x (:x pos))
               (screen-y (:y pos))
               (* (:x size) (app-size))
               (* (:y size) (app-size)))
    (.endFill graphics)))

(defn circle [{:keys [x y radius]} fill & [layer]]
  (let [graphics (get-graphics layer)]
    (.beginFill graphics fill)
    (.drawCircle graphics
                 (screen-x x)
                 (screen-y y)
                 (* radius (app-size)))
    (.endFill graphics)))

(defn polygon [points fill & [layer]]
  (let [graphics (get-graphics layer)]
    (.beginFill graphics fill)
    (.drawPolygon graphics
                  (clj->js (mapv #(pixi/Point. (screen-x (:x %))
                                               (screen-y (:y %)))
                                 points)))
    (.endFill graphics)))

(defn line [start end width color & [layer]]
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

(defn polyline [points width color & [layer]]
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

(defn text [s pos size color & [layer]]
  (when (:font-loaded? @graphics-state)
    (let [t (pixi/BitmapText.
             (str s)
             (clj->js {:fontName constants/font-name
                       :fontSize 10
                       :align "right"
                       :tint color}))
          scale (* size (text-size s))]
      (set! (.-x t)
            (- (+ (screen-x (:x pos))
                  (* scale constants/text-x-offset))
               (* (.-width t) scale 0.5)))
      (set! (.-y t)
            (- (+ (screen-y (:y pos))
                  (* scale constants/text-y-offset))
               (* (.-height t) scale 0.5)))
      (set! (.-x (.-scale t)) scale)
      (set! (.-y (.-scale t)) scale)
      (set! (.-resolution t) 10)
      (.addChild (get (:texts @graphics-state)
                      (or layer (first constants/ui-layers)))
                 t))))

(defn get-delta []
  (/ (.-elapsedMS (.-ticker (attr :app))) 1000))

(defonce current-svg-color-scheme-atom (atom nil))
(defn update-svg-color-scheme [color-scheme]
  (when (not= @current-svg-color-scheme-atom color-scheme)
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
                  (.setAttribute element attribute rgb-string)))))))))
  (reset! current-svg-color-scheme-atom color-scheme))

(defonce svg-texture-map-atom (atom {}))
(defn render-svg [name]
  (let [color-scheme (storage/color-scheme)
        name-str (cond
                   (keyword? name) (subs (str name) 1)
                   :else (str name))]
    (update-svg-color-scheme color-scheme)
    (swap! svg-texture-map-atom
           (fn [svg-texture-map]
             (update svg-texture-map
                     [name-str color-scheme]
                     (fn [texture]
                       (if texture
                         texture
                         (new pixi/Sprite
                              (pixi/Texture.from
                               (new pixi/SVGResource
                                    (.-outerHTML (js/document.getElementById name-str))))))))))
    (let [svg-sprite (@svg-texture-map-atom [name color-scheme])]
      (.addChild (.-stage (attr :app))
                 svg-sprite))))

(defn update-graphics []
  (let [app (attr :app)]
    (resize)
    (doseq [layer constants/ui-layers]
      (.clear (get-graphics layer)))
    (let [stage (.-stage app)
          texts (:texts @graphics-state)]
      (when texts
        (doseq [text-container (vals texts)]
          (.removeChild stage text-container)
          (.destroy text-container (clj->js {:children true :texture true :baseTexture true}))))
      (doseq [[layer z] (mapv vector constants/ui-layers (range))]
        (let [container (pixi/Container.)]
          (update-attr! :texts
                        #(assoc % layer container))
          (set! (.-zIndex container) (+ 0.5 z))
          (.addChild stage container))))))

(defn init [update-fn click-down-fn click-up-fn update-mouse-fn]
  (let [app (set-attr! :app
                       (pixi/Application. (clj->js {:autoResize true})))
        stage (.-stage app)]
    (set! (.-sortableChildren stage) true)
    (set-attr! :graphics {})
    (doseq [[layer z] (mapv vector constants/ui-layers (range))]
      (update-attr! :graphics
                    #(let [graphics (pixi/Graphics.)]
                       (set! (.-zIndex graphics) z)
                       (.addChild stage graphics)
                       (assoc % layer graphics))))
    (js/document.body.appendChild (.-view app))
    (.add (.-ticker app) update-fn)
    (let [interaction (get (js->clj (.-plugins (.-renderer (:app @graphics-state)))) "interaction")]
      (.on interaction "pointerdown" click-down-fn)
      (.on interaction "pointerup" click-up-fn)
      (.on interaction "pointermove" update-mouse-fn))
    (let [font (FaceFontObserver. constants/font-name)]
      (.then (.load font)
             (fn []
               (set-attr! :font-loaded? true)
               (.from pixi/BitmapFont constants/font-name
                      (clj->js
                       {:fontFamily constants/font-name
                        :fill 0xffffff
                        :fontSize 300})
                      (clj->js
                       {:chars pixi/BitmapFont.ASCII}))
               (u/log "Font loaded."))))
    
    (resize)))

(defn in-discard-corner? [pos]
  (let [[app-pos app-size] (app-rect)]
    (<= (geom/point-magnitude
         (geom/subtract-points (geom/add-points app-pos
                                                (select-keys app-size [:y]))
                               pos))
        constants/lower-corner-zone-radius)))

(defn render-discard-zone [& [highlighted? blank-symbol?]]
  (let [[app-pos app-size] (app-rect)]
    (circle (assoc (geom/add-points app-pos
                                    (select-keys app-size [:y]))
                   :radius constants/lower-corner-zone-radius)
                     (if highlighted?
                       (:highlight (storage/color-scheme))
                       (:foreground (storage/color-scheme)))
                     :menu)
    (circle (assoc (geom/add-points app-pos
                                    (select-keys app-size [:y]))
                   :radius (* (- 1 constants/corner-zone-bar-thickness)
                              constants/lower-corner-zone-radius))
            (:background (storage/color-scheme))
            :menu)
    (when blank-symbol?
      (let [radius (/ (* (- 1 constants/corner-zone-bar-thickness)
                         constants/lower-corner-zone-radius)
                      (inc (Math/sqrt 2)))
            base-circle-pos (-> app-pos
                                (update :y (partial + (- (:y app-size) radius)))
                                (update :x (partial + radius)))
            angle-offset (geom/scale-point (geom/angle-point (* 0.25 geom/PI))
                                           (* radius
                                              constants/discard-zone-icon-radius-factor))]
        (circle (assoc base-circle-pos
                       :radius (* radius
                                  constants/discard-zone-icon-radius-factor))
                (:foreground (storage/color-scheme))
                :menu)
        (circle (assoc base-circle-pos
                       :radius (* radius
                                  constants/discard-zone-icon-radius-factor
                                  (- 1 constants/discard-zone-icon-thickness)))
                (:background (storage/color-scheme))
                :menu)
        (line (geom/add-points base-circle-pos
                               angle-offset)
              (geom/subtract-points base-circle-pos
                                    angle-offset)
              (* radius
                 (* constants/discard-zone-icon-radius-factor
                    constants/discard-zone-icon-thickness))
              (:foreground (storage/color-scheme))
              :menu)))))
