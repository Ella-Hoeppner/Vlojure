(ns vlojure.app
  (:require [vlojure.graphics :as graphics]
            [vlojure.storage :as storage]
            [vlojure.util :as u]
            [vlojure.geometry :as geom]
            [vlojure.constants :as constants]
            [vlojure.vedn :as vedn]
            [vlojure.evaluation :as evaluation]
            [clojure.string :as string]))

(defonce app-state (atom {}))

(defn attr [key]
  (get @app-state key))

(defn set-attr! [key value]
  (swap! app-state
         #(assoc % key value))
  value)

(defn update-attr! [key value]
  (swap! app-state
         #(update % key value)))

(defn formbar-offset [index]
  (+ (* (storage/formbar-radius)
        (inc constants/formbar-pos))
     (* index (* 2 (storage/formbar-radius) constants/formbar-spacing))))

(defn formbar-zone-size [side]
  (let [side-stage-count (count (get (storage/project-attr :formbars) side))]
    (if (zero? side-stage-count)
      0
      (- (formbar-offset side-stage-count)
         (storage/formbar-radius)))))

(defn fill-empty-project []
  (when (zero? (count (:children (storage/project-attr :form))))
    (storage/set-project-attr! :form
                               (vedn/clj->vedn "nil"))))

(defn activate-text-page-input []
  (let [text-page-input (attr :text-page-input)]
    (set! (.-display (.-style text-page-input)) "block")
    (set! (.-value text-page-input)
          (let [form (storage/project-attr :form)]
            (apply str
                   (mapcat (fn [subform]
                             (str (vedn/vedn->clj subform)
                                  "\n\n"))
                           (:children form)))))))

(defn hide-text-page-input []
  (let [text-page-input (attr :text-page-input)]
    (set! (.-display (.-style text-page-input)) "none")))

(defn refresh-project-dropdown-input-names []
  (let [project-dropdown-input (attr :project-dropdown-input)
        option-names (mapv :name (storage/attr :projects))]
    (while (.-firstChild project-dropdown-input)
      (.removeChild project-dropdown-input
                    (.-lastChild project-dropdown-input)))
    (doseq [index (range (count option-names))]
      (let [name (nth option-names index)
            option (.createElement js/document "option")
            option-style (.-style option)]
        (set! (.-innerHTML option) name)
        (set! (.-backgroundColor option-style)
              (graphics/html-color (:highlight (storage/color-scheme))))
        (set! (.-border option-style) "none")
        (set! (.-outline option-style) "none")
        (set! (.-box-shadow option-style) "none")
        (.appendChild project-dropdown-input option)))))

(defn activate-project-rename-input []
  (let [project-rename-input (attr :project-rename-input)]
    (set! (.-value project-rename-input) (storage/project-attr :name))
    (set! (.-display (.-style project-rename-input)) "block")))

(defn hide-project-rename-input []
  (let [project-rename-input (attr :project-rename-input)]
    (set! (.-display (.-style project-rename-input)) "none")))

(defn activate-literal-text-input [path]
  (let [text-input (attr :literal-text-input)]
    (set! (.-display (.-style text-input)) "block")
    (set! (.-value text-input)
          (:value (vedn/get-child (storage/project-attr :form)
                                  path)))
    (.focus text-input))
  (set-attr! :literal-text-input-path path))

(defn hide-literal-text-input []
  (when (attr :literal-text-input-path)
    (storage/update-project-attr! :form
                              (fn [form]
                                (let [text-value (.-value (attr :literal-text-input))
                                      value (if (pos? (count text-value))
                                              text-value
                                              "nil")
                                      new-forms (:children (vedn/clj->vedn value))]
                                  (reduce #(vedn/insert-child %1
                                                              (attr :literal-text-input-path)
                                                              %2)
                                          (vedn/replace-child form
                                                              (attr :literal-text-input-path)
                                                              (first new-forms))
                                          (reverse (rest new-forms))))))
    (let [text-input (attr :literal-text-input)]
      (set! (.-display (.-style text-input)) "none")
      (set-attr! :literal-text-input-path nil))))

(defn enter-page [page]
  (set-attr! :page page)
  (when (= page :settings)
    (refresh-project-dropdown-input-names))
  (if (= page :text)
    (activate-text-page-input)
    (hide-text-page-input)))

(defn adjusted-form-circle []
  (geom/circle-within
     (graphics/app-rect)))

(defn zoomed-form-circle []
  (update (adjusted-form-circle)
          :radius
          (partial * (storage/base-zoom))))

(defn shift-layout [layout offset]
  (-> layout
      (geom/add-points offset)
      (update :sublayouts
              (fn [sublayouts]
                (mapv #(shift-layout % offset)
                      sublayouts)))))

(defn expand-layout [layout radius-factor]
  ((fn f [inner-layout]
     (-> inner-layout
         (update :radius
                 (partial * radius-factor))
         (merge (select-keys (geom/add-points layout
                                              (geom/scale-point (geom/subtract-points inner-layout
                                                                                      layout)
                                                                radius-factor))
                             [:x :y]))
         (update :sublayouts
                 #(mapv f %))))
   layout))

(defn adjust-layout [layout pos zoom]
  (-> layout
      (update :x #(* zoom (+ % (:x pos))))
      (update :y #(* zoom (+ % (:y pos))))
      (update :radius (partial * zoom))
      (update :sublayouts
              (fn [sub]
                (when sub
                  (mapv #(adjust-layout % pos zoom)
                        sub))))))

(defn map-layout [layout from to]
  ((fn f [layout {:keys [x y] :as offset} radius-change-factor]
     (let []
       (-> layout
           (update :x #(+ x (* % radius-change-factor)))
           (update :y #(+ y (* % radius-change-factor)))
           (update :radius (partial * radius-change-factor))
           (update :sublayouts
                   (fn [sub]
                     (when sub
                       (mapv #(f %
                                 offset
                                 radius-change-factor)
                             sub)))))))
   layout
   (geom/subtract-points (geom/subtract-points to (geom/scale-point geom/unit (:radius to)))
                         (geom/subtract-points from (geom/scale-point geom/unit (:radius from))))
   (/ (:radius to) (:radius from))))

(defn form-layout [form starting-layout]
  (let [current-layout starting-layout
        {:keys [type value children]} form]
    (if (= type :literal)
      (assoc current-layout
             :type type
             :value value)
      (assoc current-layout
             :type type
             :sublayouts
             (let [subform-count (count children)]
               (if (= subform-count 1)
                 [(form-layout (first children)
                               (assoc current-layout
                                      :radius (* constants/sole-subform-shrink-factor (:radius current-layout))))]
                 (let [raw-radius (Math/sin (/ Math/PI subform-count))
                       unscaled-radius (/ raw-radius (inc raw-radius))
                       radius (* unscaled-radius (- 1 constants/bubble-thickness) (:radius current-layout))]
                   (mapv (fn [subform i]
                           (let [angle (- (* Math/PI -0.5)
                                          (/ (* Math/PI 2 i) subform-count))]
                             (form-layout subform
                                          (assoc (geom/add-points current-layout
                                                                  (geom/scale-point (geom/angle-point angle)
                                                                                    (- (* (:radius current-layout)
                                                                                          (- 1 constants/bubble-thickness))
                                                                                       radius)))
                                                 :radius (* constants/subform-shrink-factor radius)))))
                         children
                         (range)))))))))

(defn settings-circle [index]
  (let [center-circle (zoomed-form-circle)
        center-radius (:radius center-circle)]
    (geom/add-points center-circle
                     (geom/scale-point (storage/attr :scroll-direction)
                                       (* (- index
                                             (attr :settings-scroll-pos))
                                          2
                                          center-radius
                                          constants/outer-form-spacing)))))

(defn settings-button-circles []
  (vec
   (let [center-circle (settings-circle 0)
         center-radius (:radius center-circle)
         bar-width (* 2 center-radius constants/settings-project-dropdown-width)
         button-radius (* bar-width constants/settings-project-button-radius)
         base-button-circle (-> center-circle
                                (update :y (partial +
                                                    (* center-radius
                                                       (+ constants/settings-project-dropdown-y
                                                          constants/settings-project-dropdown-height))
                                                    (* button-radius
                                                       (inc constants/settings-project-buttons-y-spacing))))
                                (assoc :radius button-radius))]
     (for [index (range (count constants/settings-project-buttons))]
       (update base-button-circle
               :x
               #(+ %
                   (* (- index
                         (* 0.5
                            (dec
                             (count constants/settings-project-buttons))))
                      2 button-radius
                      (inc constants/settings-project-buttons-x-spacing))))))))

(defn settings-bar-scroll-circle []
  (let [center-circle (settings-circle 1)]
    (update (geom/add-points center-circle
                             (geom/scale-point (storage/attr :scroll-direction)
                                               (* (:radius center-circle)
                                                  constants/settings-bar-scroll-circle-pos)))
            :radius
            (partial * constants/settings-bar-scroll-circle-radius))))

(defn current-form-layouts []
  (assoc geom/origin
         :sublayouts (mapv (fn [child index]
                             (form-layout child
                                          (assoc (geom/scale-point (storage/attr :scroll-direction)
                                                                   (* constants/outer-form-spacing
                                                                      index
                                                                      2))
                                                 :radius 1)))
                           (:children (storage/project-attr :form))
                           (range))
         :radius ##Inf))

(defn adjusted-form-layouts []
  (let [form-layouts (current-form-layouts)
        current-app-rect (graphics/app-rect)
        form-circle (adjusted-form-circle)]
    (reduce (fn [layouts index]
              (update-in layouts
                         [:sublayouts index]
                         #(-> %
                              (adjust-layout (attr :camera-pos)
                                             (attr :camera-zoom))
                              (adjust-layout geom/unit 0.5)
                              (map-layout (geom/circle-within current-app-rect)
                                          form-circle))))
            form-layouts
            (range (count (:sublayouts form-layouts))))))

(defn get-sublayout [layout path]
  (if (empty? path)
    layout
    (get-sublayout (nth (:sublayouts layout)
                        (first path))
                   (rest path))))

(defn on-settings-page? [index]
  (and (= (attr :page) :settings)
       (< (Math/abs (- (attr :ideal-settings-scroll-pos) index)) 0.01)))

(defn resize-html-elements []
  (let [current-size (graphics/app-size)
        {:keys [page
                literal-text-input
                project-dropdown-input
                project-rename-input
                text-page-input]} @app-state]
    (when literal-text-input
      (let [{:keys [literal-text-input-path]} @app-state
            {:keys [x y radius]} (when literal-text-input-path
                                   (get-sublayout (adjusted-form-layouts)
                                                  literal-text-input-path))
            text-input-str (.-value literal-text-input)
            length (count text-input-str)
            new-text-size (* constants/html-text-size-factor
                             radius
                             (graphics/text-size text-input-str))

            new-text-width (* new-text-size length)
            new-text-height (* constants/html-text-height-factor new-text-size)]
        (set! (.-left (.-style literal-text-input))
              (str (int (+ (- (graphics/screen-x x)
                              (/ new-text-width 2))
                           (* constants/html-text-x-offset new-text-size)))
                   "px"))
        (set! (.-top (.-style literal-text-input))
              (str (int (+ (- (graphics/screen-y y)
                              (/ new-text-height 2))
                           (* constants/html-text-y-offset new-text-size)))
                   "px"))
        (set! (.-width (.-style literal-text-input))
              (str new-text-width "px"))
        (set! (.-height (.-style literal-text-input))
              (str new-text-height "px"))
        (set! (.-fontSize (.-style literal-text-input))
              (str new-text-size
                   "px"))))
    (when project-dropdown-input
      (let [{:keys [x y radius]} (settings-circle 0)
            dropdown-width (* current-size
                              radius
                              constants/settings-project-dropdown-width)
            new-text-size (* current-size
                             constants/text-scale-factor
                             constants/html-text-size-factor
                             constants/settings-project-name-size
                             radius)
            style (.-style project-dropdown-input)]
        (set! (.-display style)
              (if (and (= (.-display (.-style project-rename-input))
                          "none")
                       (on-settings-page? 0))
                "block"
                "none"))
        (set! (.-left style)
              (str (- (graphics/screen-x x)
                      dropdown-width)
                   "px"))
        (set! (.-top style)
              (str (graphics/screen-y
                    (+ y (* constants/settings-project-dropdown-y radius)))
                   "px"))
        (set! (.-width style)
              (str (* 2 dropdown-width)
                   "px"))
        (set! (.-height style)
              (str (* current-size
                      radius
                      constants/settings-project-dropdown-height)
                   "px"))
        (set! (.-fontSize style)
              (str new-text-size
                   "px"))))
    (when project-rename-input
      (let [{:keys [x y radius]} (settings-circle 0)
            dropdown-width (* current-size
                              radius
                              constants/settings-project-dropdown-width)
            new-text-size (* current-size
                             constants/text-scale-factor
                             constants/html-text-size-factor
                             constants/settings-project-name-size
                             radius)
            style (.-style project-rename-input)]
        (when (and (not (on-settings-page? 0))
                   (= (.-display (.-style project-rename-input))
                      "block"))
          (refresh-project-dropdown-input-names)
          (hide-project-rename-input))
        (set! (.-left style)
              (str (- (graphics/screen-x x)
                      dropdown-width)
                   "px"))
        (set! (.-top style)
              (str (graphics/screen-y
                    (+ y (* constants/settings-project-dropdown-y radius)))
                   "px"))
        (set! (.-width style)
              (str (* 2 dropdown-width)
                   "px"))
        (set! (.-height style)
              (str (* current-size
                      radius
                      constants/settings-project-dropdown-height)
                   "px"))
        (set! (.-fontSize style)
              (str new-text-size
                   "px"))))
    (when text-page-input
      (let [width (graphics/app-width)
            height (graphics/app-height)
            left-x (graphics/screen-x (+ (:x (first (graphics/app-rect)))
                                constants/text-page-border))
            top-y (graphics/screen-y (+ (:y (first (graphics/app-rect)))
                               constants/text-page-border))]
        (set! (.-left (.-style text-page-input))
              (str left-x
                   "px"))
        (set! (.-width (.-style text-page-input))
              (str (- (graphics/screen-x (- (:x (apply geom/add-points (graphics/app-rect)))
                                   constants/text-page-border))
                      top-y)
                   "px"))
        (set! (.-top (.-style text-page-input))
              (str top-y
                   "px"))
        (set! (.-height (.-style text-page-input))
              (str (- (graphics/screen-y (- (:y (apply geom/add-points (graphics/app-rect)))
                                   constants/text-page-border))
                      top-y)
                   "px"))))))

(defn layout-path-encapsulated? [layout path]
  (boolean
   (u/in? vedn/encapsulator-types
          (:type (get-sublayout layout (butlast path))))))

(defn layout-path-at [layout pos]
  (when (geom/in-circle? layout pos)
    (let [{:keys [sublayouts]} layout]
      (if (empty? sublayouts)
        '()
        (or (some (fn [i]
                    (let [sublayout (nth sublayouts i)
                          sub-path (layout-path-at sublayout pos)]
                      (when sub-path
                        (conj sub-path i))))
                  (range (count sublayouts)))
            '())))))

(defn layout-insertion-path-at [layout pos]
  (when (geom/in-circle? layout pos)
    (let [{:keys [sublayouts]} layout]
      (if (empty? sublayouts)
        '()
        (or (some (fn [i]
                    (let [sublayout (nth sublayouts i)
                          sub-path (layout-insertion-path-at sublayout pos)]
                      (when sub-path
                        (conj sub-path i))))
                  (range (count sublayouts)))
            (let [sub-count (count sublayouts)
                  mouse-angle (mod (geom/point-angle (geom/subtract-points pos layout))
                                   geom/TAU)
                  upper-angle (* geom/TAU 0.75)
                  angle-offset (- mouse-angle upper-angle)]
              (if (< (Math/abs (- mouse-angle upper-angle))
                     (/ geom/TAU (* 3 sub-count)))
                (list -1)
                (list (int (/ (* sub-count
                                 (mod (- angle-offset)
                                      geom/TAU))
                              geom/TAU))))))))))

(defn color-scheme-index-at [pos]
  (if (on-settings-page? 2)
    (let [center-circle (settings-circle 2)
          center-radius (:radius center-circle)]
      (some (fn [index]
              (let [y (* center-radius
                         (+ constants/settings-color-text-y
                            (* index constants/settings-color-spacing)))
                    height (* center-radius
                              constants/settings-color-height)
                    width (* center-radius
                             2
                             constants/settings-color-width)]
                (when (or (geom/in-rect? [(-> center-circle
                                              (update :y (partial + y (* -0.5 height)))
                                              (update :x (partial + (* -0.5 width))))
                                          {:x width
                                           :y height}]
                                         pos)
                          (geom/in-circle? (-> center-circle
                                               (update :y (partial + y))
                                               (update :x (partial + (* -0.5 width)))
                                               (assoc :radius (* 0.5 height)))
                                         pos)
                          (geom/in-circle? (-> center-circle
                                               (update :y (partial + y))
                                               (update :x (partial + (* 0.5 width)))
                                               (assoc :radius (* 0.5 height)))
                                           pos))
                  index)))
            (range (count constants/color-schemes))))
    nil))

(defn ideal-camera-pos []
  (geom/scale-point
   (let [scroll-dir (storage/attr :scroll-direction)
         total-layout (-> (current-form-layouts)
                          (adjust-layout (geom/scale-point scroll-dir
                                                           (* constants/outer-form-spacing
                                                              2
                                                              (attr :scroll-pos)))
                                         1))
         {:keys [selected-layout-path]} @app-state
         sublayout (get-sublayout total-layout
                                  selected-layout-path)]
     (if (pos? (count selected-layout-path))
       (geom/add-points sublayout
                        (geom/scale-point scroll-dir
                                          (* constants/outer-form-spacing
                                             -2
                                             (first selected-layout-path))))
       sublayout))
   -1))

(defn ideal-camera-zoom []
  (let [{:keys [selected-layout-path]} @app-state]
    (if selected-layout-path
      (/ (storage/base-zoom)
         (:radius
          (get-sublayout (current-form-layouts)
                         selected-layout-path)))
      (storage/base-zoom))))

(defn flatten-layout [layout]
  (if (:sublayouts layout)
    (conj (mapcat flatten-layout
                  (:sublayouts layout))
          (dissoc layout :sublayouts))
    (list layout)))

(defn render-layout [layout & [layer]]
  (let [center layout
        radius (:radius layout)]
    (when (#{:list :map :set :lit-fn :literal} (:type layout))
      (graphics/circle layout
                       (:foreground (storage/color-scheme))
                       layer))
    (when (#{:map :lit-fn} (:type layout))
      (let [r (:radius layout)]
        (doseq [base-angle [(* geom/PI 0.25)
                            (* geom/PI 0.75)
                            (* geom/PI 1.25)
                            (* geom/PI 1.75)]]
          (graphics/polygon [(geom/add-points layout
                                              (geom/scale-point (geom/angle-point (- base-angle constants/map-point-width))
                                                                r))
                             (geom/add-points layout
                                              (geom/scale-point (geom/angle-point base-angle)
                                                                (* (inc constants/map-point-height) r)))
                             (geom/add-points layout
                                              (geom/scale-point (geom/angle-point (+ base-angle constants/map-point-width))
                                                                r))]
                            (:foreground (storage/color-scheme))
                            layer))))
    (when (#{:set :lit-fn} (:type layout))
      (let [r (:radius layout)]
        (doseq [angle [0
                       (* geom/PI 0.5)
                       geom/PI
                       (* geom/PI 1.5)]]
          (let [base-offset (geom/scale-point (geom/angle-point (+ angle (* geom/PI 0.5)))
                                              (* r constants/set-line-offset))]
            (doseq [offset [base-offset (geom/scale-point base-offset -1)]]
              (graphics/line (geom/add-points offset layout)
                             (geom/add-points layout
                                              offset
                                              (geom/scale-point (geom/angle-point angle)
                                                                (* (inc constants/set-line-length) r)))
                             (* r constants/set-line-width)
                             (:foreground (storage/color-scheme))
                             layer))))))
    (when (#{:list :map :set :lit-fn} (:type layout))
      (graphics/circle (update layout
                               :radius (partial *
                                                (- 1 constants/bubble-thickness)))
                       (:background (storage/color-scheme))
                       layer))
    (when (= (:type layout) :vector)
      (graphics/polygon (mapv #(geom/add-points center
                                                (geom/scale-point %
                                                                  (* radius
                                                                     constants/vector-size-factor)))
                              (geom/polygon 8
                                            (* geom/PI 0.125)))
                        (:foreground (storage/color-scheme))
                        layer)
      (graphics/polygon (mapv #(geom/add-points center
                                                (geom/scale-point %
                                                                  (* radius
                                                                     constants/vector-size-factor
                                                                     (- 1 constants/bubble-thickness))))
                              (geom/polygon 8
                                            (* geom/PI 0.125)))
                        (:background (storage/color-scheme))
                        layer))
    (when (= (:type layout) :quote)
      (let [radius (:radius layout)]
        (doseq [angle (mapv (partial * geom/TAU) (u/prop-range constants/quote-divs true))]
          (let [[start end]
                (map #(geom/add-points layout
                                       (geom/scale-point (geom/angle-point
                                                          (+ angle
                                                             (* % (/ geom/TAU constants/quote-divs 4))))
                                                         radius))
                     [-1 1])]
            (graphics/line start end
                           (* radius
                              constants/bubble-thickness)
                           (:foreground (storage/color-scheme))
                           layer)))))
    (when (= (:type layout) :deref)
      (let [radius (:radius layout)]
        (doseq [angle (mapv (partial * geom/TAU) (u/prop-range constants/deref-circles true))]
          (graphics/circle (update (geom/add-points layout
                                                    (geom/scale-point (geom/angle-point angle)
                                                                      radius))
                                   :radius
                                   (partial * constants/deref-circle-size-factor))
                           (:foreground (storage/color-scheme))
                           layer))))
    (when (= (:type layout) :syntax-quote)
      (let [radius (:radius layout)]
        (doseq [angle (mapv (partial * geom/TAU) (u/prop-range constants/syntax-quote-divs true))]
          (let [[start end]
                (map #(geom/add-points layout
                                       (geom/scale-point (geom/angle-point
                                                          (+ angle
                                                             (* % (/ geom/TAU constants/syntax-quote-divs 4))))
                                                         (* radius
                                                            (+ 1
                                                               (* % constants/syntax-quote-offset-factor)))))
                     [-1 1])]
            (graphics/line start end
                           (* radius
                              constants/bubble-thickness)
                           (:foreground (storage/color-scheme))
                           layer)))))
    (when (= (:type layout) :comment)
      (let [radius (:radius layout)]
        (doseq [angle (mapv (partial * geom/TAU) (u/prop-range constants/comment-divs true))]
          (graphics/line (geom/add-points layout
                                          (geom/scale-point (geom/angle-point angle)
                                                            radius))
                         (geom/add-points layout
                                          (geom/scale-point (geom/angle-point angle)
                                                            (* radius (- 1 constants/comment-length-factor))))
                         (* radius
                            constants/bubble-thickness)
                         (:foreground (storage/color-scheme))
                         layer))))
    (when (= (:type layout) :unquote)
      (let [segment-angle (/ geom/TAU 8)
            angles (mapv #(+ (* segment-angle %)
                             (/ geom/TAU 16))
                         (range 8))
            points (mapv #(geom/scale-point (geom/angle-point %)
                                            (* radius
                                               constants/vector-size-factor))
                         angles)]
        (doseq [[start-point end-point] (partition 2 1 (conj points (first points)))]
          (let [div-size (/ 0.5 constants/unquote-divs)
                tween-starts (mapv #(* div-size (+ 0.5 (* 2 %)))
                                   (range constants/unquote-divs))]
            (doseq [tween-start tween-starts]
              (graphics/line (geom/add-points layout
                                              (geom/tween-points start-point end-point tween-start))
                             (geom/add-points layout
                                              (geom/tween-points start-point end-point (+ tween-start div-size)))
                             (* radius
                                constants/bubble-thickness)
                             (:foreground (storage/color-scheme))
                             layer))))))
    (when (= (:type layout) :unquote-splice)
      (let [segment-angle (/ geom/TAU 8)
            angles (mapv #(+ (* segment-angle %)
                             (/ geom/TAU 16))
                         (range 8))
            points (mapv #(geom/scale-point (geom/angle-point %)
                                            (* radius
                                               constants/vector-size-factor))
                         angles)]
        (doseq [[start-point end-point] (partition 2 1 (conj points (first points)))]
          (let [div-spacing (/ 0.5 constants/unquote-splice-circles)]
            (doseq [t (u/prop-range constants/unquote-splice-circles true)]
              (graphics/circle (update (geom/add-points layout
                                                        (geom/tween-points start-point end-point t))
                                       :radius
                                       (partial * constants/deref-circle-size-factor))
                               (:foreground (storage/color-scheme))
                               layer))))))
    (when (= (:type layout) :meta)
      (let [radius (:radius layout)
            angle-offset (/ geom/PI 2 constants/meta-divs)]
        (doseq [angle (mapv (partial * geom/TAU) (u/prop-range constants/meta-divs true))]
          (let [tip (geom/add-points layout
                                     (geom/scale-point (geom/angle-point angle)
                                                       radius))
                [start end]
                (mapv #(geom/add-points layout
                                        (geom/scale-point (geom/angle-point (+ angle (* % angle-offset)))
                                                          (* radius (- 1 constants/meta-length-factor))))
                      [1 -1])]
            (graphics/polyline [start tip end]
                               (* radius
                                  constants/bubble-thickness)
                               (:foreground (storage/color-scheme))
                               layer)))))
    (when (= (:type layout) :var-quote)
      (let [radius (:radius layout)]
        (doseq [angle (mapv (partial * geom/TAU) (u/prop-range constants/var-quote-divs true))]
          (let [[start end]
                (map #(geom/add-points layout
                                       (geom/scale-point (geom/angle-point
                                                          (+ angle
                                                             (* % (/ geom/TAU constants/var-quote-divs 4))))
                                                         radius))
                     [-1 1])]
            (graphics/line start
                           end
                           (* radius
                              constants/bubble-thickness)
                           (:foreground (storage/color-scheme))
                           layer))
          (let [[start end]
                (map #(geom/add-points layout
                                       (geom/scale-point (geom/angle-point angle)
                                                         (* radius %)))
                     [1 (inc constants/var-quote-length)])]
            (graphics/line start
                           end
                           (* radius
                              constants/bubble-thickness)
                           (:foreground (storage/color-scheme))
                           layer)))))
    (when (= (:type layout) :literal)
      (graphics/text (:value layout)
                     layout
                     (:radius layout)
                     (:text (storage/color-scheme))
                     layer))))

(defn formbar-arrangement []
  (zipmap
   constants/screen-sides
   (map (fn [side]
          (let [[app-pos app-size] (graphics/app-rect)
                formbars (storage/project-attr :formbars)
                horizontal? (#{:top :bottom} side)
                bar-stages (get formbars side)]
            (mapv (fn [stage stage-index]
                    (let [sizes (mapv (fn [bar]
                                        (max 0
                                             (let [size (count (:forms bar))]
                                               (* (dec size)
                                                  2
                                                  (storage/formbar-radius)
                                                  (- 1 constants/formbar-outline-thickness)))))
                                      stage)
                          total-size (+ (apply + sizes)
                                        (* (inc constants/formbar-spacing)
                                           (storage/formbar-radius)
                                           (dec (count stage))))
                          edge-offset (formbar-offset stage-index)
                          offsets (reduce (fn [offsets size]
                                            (conj offsets
                                                  (+ (last offsets)
                                                     size
                                                     (* (inc constants/formbar-spacing)
                                                        (storage/formbar-radius)))))
                                          [(* -0.5 total-size)]
                                          sizes)]
                      (mapv (fn [bar size bar-offset]
                              (let [bar-pos (case side
                                              :bottom {:x (+ (:x app-pos) (* 0.5 (:x app-size)) bar-offset)
                                                       :y (- (+ (:y app-pos) (:y app-size)) edge-offset)}
                                              :top {:x (+ (:x app-pos) (* 0.5 (:x app-size)) bar-offset)
                                                    :y (+ (:y app-pos) edge-offset)}
                                              :left {:x (+ (:x app-pos) edge-offset)
                                                     :y (+ (:y app-pos) (* 0.5 (:y app-size)) bar-offset)}
                                              :right {:x (- (+ (:x app-pos) (:x app-size)) edge-offset)
                                                      :y (+ (:y app-pos) (* 0.5 (:y app-size)) bar-offset)})]
                                (merge bar-pos
                                       {:width (if horizontal? size 0)
                                        :height (if horizontal? 0 size)
                                        :circles
                                        (mapv (fn [form form-index]
                                                (assoc (geom/add-points bar-pos
                                                                        (geom/scale-point
                                                                         {(if horizontal? :x :y)
                                                                          (* 2
                                                                             (- 1 constants/formbar-outline-thickness)
                                                                             (storage/formbar-radius))}
                                                                         form-index))
                                                       :radius (* (storage/formbar-radius)
                                                                  (- 1 constants/formbar-outline-thickness)
                                                                  constants/formbar-form-size)
                                                       :form form))
                                              (:forms bar)
                                              (range))})))
                            stage
                            sizes
                            offsets)))
                  bar-stages
                  (range))))
        constants/screen-sides)))

(defn formbar-path-at [pos]
  (let [full-arrangement (formbar-arrangement)]
    (some (fn [side]
            (let [arrangement (get full-arrangement side)
                  horizontal? (#{:top :bottom} side)]
              (some (fn [stage-index]
                      (let [stage (nth arrangement stage-index)]
                        (some (fn [bar-index]
                                (let [bar (nth stage bar-index)]
                                  (when (or (<= (geom/point-magnitude
                                                 (geom/subtract-points pos
                                                                       bar))
                                                (storage/formbar-radius))
                                            (<= (geom/point-magnitude
                                                 (geom/subtract-points pos
                                                                       (geom/add-points bar
                                                                                        (if horizontal?
                                                                                          {:x (:width bar)}
                                                                                          {:y (:height bar)}))))
                                                (storage/formbar-radius))
                                            (geom/in-rect? (if horizontal?
                                                             [(geom/subtract-points bar
                                                                                    {:y (storage/formbar-radius)})
                                                              {:x (:width bar)
                                                               :y (* 2 (storage/formbar-radius))}]
                                                             [(geom/subtract-points bar
                                                                                    {:x (storage/formbar-radius)})
                                                              {:x (* 2 (storage/formbar-radius))
                                                               :y (:height bar)}])
                                                           pos))
                                    [side stage-index bar-index])))
                              (range (count stage)))))
                    (range (count arrangement)))))
          constants/screen-sides)))

(defn formbar-form-path-at [pos]
  (let [full-arrangement (formbar-arrangement)]
    (some (fn [side]
            (let [arrangement (get full-arrangement side)]
              (some (fn [stage-index]
                      (let [stage (nth arrangement stage-index)]
                        (some (fn [bar-index]
                                (let [bar (nth stage bar-index)
                                      {:keys [circles]} bar]
                                  (some (fn [circle-index]
                                          (let [bar-circle (nth circles circle-index)]
                                            (when (geom/in-circle? bar-circle pos)
                                              [side stage-index bar-index :circles circle-index :form])))
                                        (range (count circles)))))
                              (range (count stage)))))
                    (range (count arrangement)))))
          constants/screen-sides)))

(defn new-formbar-circles []
  (let [[app-pos app-size] (graphics/app-rect)
        arrangement (formbar-arrangement)]
    (apply hash-map
           (mapcat (fn [side]
                     (concat
                      (let [side-formbars (get arrangement side)]
                        (mapcat (fn [stage-index]
                                  (let [stage (nth side-formbars stage-index)
                                        dim (if (#{:left :right} side)
                                              :y
                                              :x)
                                        size-attribute (if (#{:left :right} side)
                                                         :height
                                                         :width)]
                                    (list
                                     (assoc (update (first stage)
                                                    dim
                                                    #(- %
                                                        (* (storage/formbar-radius)
                                                           (inc constants/new-formbar-circle-radius)
                                                           constants/formbar-spacing)))
                                            :radius (* constants/new-formbar-circle-radius
                                                       (storage/formbar-radius)))
                                     [side stage-index 0]
                                     (assoc (update (last stage)
                                                    dim
                                                    #(+ %
                                                        (size-attribute (last stage))
                                                        (* (storage/formbar-radius)
                                                           (inc constants/new-formbar-circle-radius)
                                                           constants/formbar-spacing)))
                                            :radius (* constants/new-formbar-circle-radius
                                                       (storage/formbar-radius)))
                                     [side stage-index (count stage)])))
                                (range (count side-formbars))))
                      (let [side-center (geom/add-points app-pos
                                                         (case side
                                                           :top {:x (* 0.5 (:x app-size))}
                                                           :bottom (update app-size :x (partial * 0.5))
                                                           :left {:y (* 0.5 (:y app-size))}
                                                           :right (update app-size :y (partial * 0.5))))
                            perpendicular-direction (case side
                                                      :top {:y 1}
                                                      :bottom {:y -1}
                                                      :left {:x 1}
                                                      :right {:x -1})
                            arrangement-side (get arrangement side)]
                        [(assoc (geom/add-points side-center
                                                 (geom/scale-point perpendicular-direction
                                                                   (- (+ (* constants/new-formbar-circle-radius
                                                                            (storage/formbar-radius))
                                                                         (formbar-offset (count arrangement-side)))
                                                                      (storage/formbar-radius))))
                                :radius (* constants/new-formbar-circle-radius
                                           (storage/formbar-radius)))
                         [side (count arrangement-side) 0]])))
                   constants/screen-sides))))

(defn new-formbar-circle-path-at [pos]
  (let [circles (new-formbar-circles)]
    (some (fn [[new-formbar-circle path]]
            (when (geom/in-circle? new-formbar-circle pos)
              path))
          circles)))

(defn settings-slider-at [pos]
  (some (fn [index]
          (let [settings-circle (settings-circle 1)
                left (-> settings-circle
                         (update :radius (partial * constants/settings-slider-radius))
                         (update :x #(- % (* (:radius settings-circle)
                                             constants/settings-slider-width)))
                         (update :y (partial +
                                             (* (:radius settings-circle)
                                                (+ constants/settings-top-slider-y
                                                   (* constants/settings-slider-spacing index))))))
                right (-> settings-circle
                          (update :radius (partial * constants/settings-slider-radius))
                          (update :x (partial +
                                              (* (:radius settings-circle)
                                                 constants/settings-slider-width)))
                          (update :y (partial +
                                              (* (:radius settings-circle)
                                                 (+ constants/settings-top-slider-y
                                                    (* constants/settings-slider-spacing index))))))
                slider-rect [(update left :y #(- % (* (:radius settings-circle)
                                                      constants/settings-slider-radius)))
                             {:x (apply - (map :x [right left]))
                              :y (* 2 (:radius settings-circle)
                                    constants/settings-slider-radius)}]]
            (when (or (geom/in-circle? right
                                       pos)
                      (geom/in-circle? left
                                       pos)
                      (geom/in-rect? slider-rect
                                     pos))
              index)))
        (range (count constants/settings-sliders))))

(defn settings-circle-at [pos]
  (some #(when (geom/in-circle? (settings-circle %)
                                pos)
           %)
        (range constants/settings-pages)))

(defn get-mouse-zone []
  (let [[app-pos app-size] (graphics/app-rect)
        {:keys [mouse eval-zone-radius]} @app-state]
    (case (attr :page)
      :code
      (cond
        (<= (geom/point-magnitude
             (geom/subtract-points app-pos
                                   mouse))
            constants/upper-corner-zone-radius)
        :settings-icon

        (<= (geom/point-magnitude
             (geom/subtract-points (geom/add-points app-pos
                                                    (select-keys app-size [:x]))
                                   mouse))
            constants/upper-corner-zone-radius)
        :text-icon

        (<= (geom/point-magnitude
             (geom/subtract-points (geom/add-points app-pos
                                                    (select-keys app-size [:y]))
                                   mouse))
            constants/lower-corner-zone-radius)
        :discard

        (<= (geom/point-magnitude
             (geom/subtract-points (geom/add-points app-pos
                                                    app-size)
                                   mouse))
            eval-zone-radius)
        :eval

        (formbar-path-at mouse)
        :formbar

        (reduce #(or %1
                     (geom/in-circle? %2 mouse))
                false
                (:sublayouts
                 (adjusted-form-layouts)))
        :program

        :else :empty)

      :settings
      (let [button-circles (settings-button-circles)]
        (or (when (on-settings-page? 0)
              (some (fn [index]
                    (when (geom/in-circle? (nth button-circles index)
                                           mouse)
                      (nth constants/settings-project-buttons index)))
                  (range (count button-circles))))
            (cond
              (<= (geom/point-magnitude
                   (geom/subtract-points app-pos
                                         mouse))
                  constants/upper-corner-zone-radius)
              :settings-icon

              (formbar-path-at mouse)
              :formbar

              (new-formbar-circle-path-at mouse)
              :new-formbar

              (and (on-settings-page? 1)
                   (geom/in-circle? (settings-bar-scroll-circle) mouse))
              :scroll-circle

              (and (on-settings-page? 1)
                   (settings-slider-at mouse))
              :settings-slider

              (color-scheme-index-at mouse)
              :color-scheme

              (settings-circle-at mouse)
              :settings-circle

              :else :empty)))

      :text
      (cond
        (<= (geom/point-magnitude
             (geom/subtract-points app-pos
                                   mouse))
            constants/upper-corner-zone-radius)
        :back-icon

        :else :empty)

      :empty)))

(defn render-sublayouts [layout & [layer]]
  (doseq [sublayout (flatten-layout layout)]
    (render-layout sublayout layer)))

(defn mouse-dragging? []
  (> (:drag-dist (attr :mouse)) constants/min-drag-dist))

(defn placement-form []
  (let [{:keys [mouse]} @app-state
        {:keys [down? down-path down-zone down-formbar-form-path]} mouse]
    (when down?
      (cond (= down-zone :program)
            (vedn/get-child (storage/project-attr :form)
                            down-path)

            (= down-zone :eval)
            (let [last-result (first (storage/project-attr :eval-results))]
              (when (not= last-result :error)
                last-result))

            (= down-zone :discard)
            (first (storage/project-attr :discard-history))

            down-formbar-form-path
            (get-in (formbar-arrangement) down-formbar-form-path)))))

(defn get-formbar-insertion-index []
  (let [{:keys [mouse]} @app-state
        formbar-path (formbar-path-at mouse)]
    (when formbar-path-at
      (let [arrangement (formbar-arrangement)
            screen-side (first formbar-path)
            bar-arrangement (get-in arrangement formbar-path)
            form-spacing (* (storage/formbar-radius)
                            (- 1 constants/formbar-outline-thickness))]
        (if (zero? (count (:circles bar-arrangement)))
              0
              (let [horizontal? (#{:top :bottom} screen-side)
                    offset (/ (if horizontal?
                                (- (:x mouse) (:x bar-arrangement))
                                (- (:y mouse) (:y bar-arrangement)))
                              (* 2 form-spacing))]
                (if (neg? offset)
                  0
                  (inc (int offset)))))))))

(defn render-formbars []
  (let [{:keys [mouse]} @app-state
        arrangement (formbar-arrangement)
        formbar-form-path (formbar-form-path-at mouse)]
    (doseq [side constants/screen-sides]
      (let [horizontal? (#{:top :bottom} side)
            side-arrangement (get arrangement side)]
        (doseq [stage side-arrangement]
          (doseq [bar stage]
            (doseq [[color radius-factor]
                    [[(:foreground (storage/color-scheme)) 1]
                     [(:background (storage/color-scheme)) (- 1 constants/formbar-outline-thickness)]]]
              (graphics/circle (assoc bar :radius (* (storage/formbar-radius) radius-factor))
                               color
                               :formbar)
              (if horizontal?
                (do (graphics/circle (assoc (geom/add-points bar
                                                             {:x (:width bar)})
                                            :radius (* (storage/formbar-radius) radius-factor))
                                     color
                                     :formbar)
                    (graphics/rect [(geom/subtract-points bar {:y (* (storage/formbar-radius) radius-factor)})
                                    {:x (:width bar)
                                     :y (* 2 (storage/formbar-radius) radius-factor)}]
                                   color
                                   :formbar))
                (do (graphics/circle (assoc (geom/add-points bar
                                                             {:y (:height bar)})
                                            :radius (* (storage/formbar-radius) radius-factor))
                                     color
                                     :formbar)
                    (graphics/rect [(geom/subtract-points bar {:x (* (storage/formbar-radius) radius-factor)})
                                    {:x (* 2 (storage/formbar-radius) radius-factor)
                                     :y (:height bar)}]
                                   color
                                   :formbar))))))
        (when (and formbar-form-path
                   (= (first formbar-form-path) side))
          (graphics/circle (update (get-in arrangement
                                           (butlast (formbar-form-path-at mouse)))
                                   :radius (partial * (/ constants/formbar-form-size)))
                           (:highlight (storage/color-scheme))
                           :formbar))
        (doseq [stage side-arrangement]
          (doseq [bar stage]
            (doseq [bar-circle (:circles bar)]
              (render-sublayouts (form-layout (:form bar-circle)
                                              bar-circle)
                                 :formbar))))))))

(defn outer-form-insertion-index []
  (let [{:keys [mouse page]} @app-state
        zone (get-mouse-zone)
        first-sublayout (first
                         (:sublayouts
                          (adjusted-form-layouts)))]
    (when (and (= page :code)
               (= zone :empty))
      (let [projection (geom/scalar-point-projection (geom/subtract-points mouse
                                                                           first-sublayout)
                                                     (storage/attr :scroll-direction))
            rejection (geom/scalar-point-rejection (geom/subtract-points mouse
                                                                         first-sublayout)
                                                   (storage/attr :scroll-direction))
            pos (/ projection
                   (* (storage/base-zoom)
                      constants/outer-form-spacing))]
        (when (< (Math/abs rejection)
                 (* 0.5 (storage/base-zoom)))
          (if (neg? pos)
            -1
            (int pos)))))))

(defn render-app-state []
  (let [current-app-rect (graphics/app-rect)
        [app-pos app-size] current-app-rect
        mouse-zone (get-mouse-zone)
        current-placement-form (placement-form)]
    (graphics/rect current-app-rect
                   (:background (storage/color-scheme))
                   :background)
    (case (attr :page)
      :code
      (do
        ;; Handle literal text input
        (let [{:keys [literal-text-input-path]} @app-state]
          (when literal-text-input-path
            (graphics/circle (get-sublayout (adjusted-form-layouts)
                                            literal-text-input-path)
                             (:highlight (storage/color-scheme))
                             :menu)))

        (let [{:keys [mouse]} @app-state
              dragging? (mouse-dragging?)
              current-outer-form-insertion-index (outer-form-insertion-index)]
          (render-sublayouts (adjusted-form-layouts)
                             :program)
          (when (and dragging? (placement-form))
            (graphics/circle (assoc mouse
                                    :radius constants/drag-cursor-radius)
                             (:highlight (storage/color-scheme))
                             :drag))
          (when (and dragging?
                     current-placement-form
                     current-outer-form-insertion-index)
            (let [sublayouts (:sublayouts
                              (adjusted-form-layouts))
                  start? (= current-outer-form-insertion-index -1)
                  adjusted-form-layout (if start?
                                         (first sublayouts)
                                         (nth sublayouts
                                              (min current-outer-form-insertion-index
                                                   (dec (count sublayouts)))))
                  {:keys [radius]} adjusted-form-layout
                  scroll-direction (storage/attr :scroll-direction)
                  base-circle (select-keys (expand-layout (shift-layout adjusted-form-layout
                                                                        (geom/scale-point scroll-direction
                                                                                          (* (if start? -1 1)
                                                                                             constants/outer-form-spacing
                                                                                             radius)))
                                                          constants/drop-form-radius-factor)
                                           [:x :y :radius])]
              (graphics/circle (update base-circle :radius (partial * constants/drop-form-outline-radius-factor))
                               (:background (storage/color-scheme))
                               :drag)
              (render-sublayouts (form-layout current-placement-form
                                              base-circle)
                                 :drag)))
          (let [layout-path (layout-path-at (adjusted-form-layouts)
                                            (attr :mouse))
                insertion-path (layout-insertion-path-at (adjusted-form-layouts)
                                                         (attr :mouse))
                literal? (and (= (count layout-path) (count insertion-path))
                              (= :literal (:type (vedn/get-child (storage/project-attr :form)
                                                                 insertion-path))))
                layout-encapsulated? (layout-path-encapsulated? (adjusted-form-layouts)
                                                                insertion-path)]
            (when (and layout-path (not dragging?) literal?)
              (graphics/circle (get-sublayout (adjusted-form-layouts)
                                              layout-path)
                               (:highlight (storage/color-scheme))
                               :program))
            (when (and layout-path current-placement-form)
              (let [sublayout (get-sublayout (adjusted-form-layouts) layout-path)]
                (when dragging?
                  (let [parent-radius (:radius sublayout)
                        radius (* constants/drop-form-radius-factor
                                  (:radius (first (:sublayouts sublayout))))
                        child-count (count (:sublayouts sublayout))
                        last-insertion-index (last insertion-path)
                        placement-pos (if (and (not literal?)
                                               layout-encapsulated?)
                                        (get-sublayout (adjusted-form-layouts) layout-path)
                                        (if (= last-insertion-index -1)
                                          (geom/add-points sublayout
                                                           {:y (- radius
                                                                  (:radius sublayout))})
                                          (geom/add-points sublayout
                                                           (geom/scale-point (geom/angle-point
                                                                              (- (* (+ (/ (+ last-insertion-index 0.5)
                                                                                          child-count)
                                                                                       0.25)
                                                                                    geom/TAU)))
                                                                             (- parent-radius
                                                                                (* constants/drop-form-offset-factor
                                                                                   radius))))))]
                    (when (= mouse-zone :program)
                      (graphics/line mouse
                                     placement-pos
                                     constants/drag-cursor-line-width
                                     (:highlight (storage/color-scheme))
                                     :drag-forms)
                      (let [base-sublayout (form-layout (placement-form)
                                                        (assoc geom/origin :radius 1))]
                        (if literal?
                          (render-sublayouts (adjust-layout base-sublayout
                                                            (geom/scale-point sublayout
                                                                              (/ (:radius sublayout)))
                                                            (:radius sublayout))
                                             :drag-forms)
                          (if layout-encapsulated?
                            (let [encapsulated-sublayout (get-sublayout (adjusted-form-layouts) layout-path)]
                              (render-sublayouts (adjust-layout base-sublayout
                                                                (geom/scale-point encapsulated-sublayout
                                                                                  (/ (:radius encapsulated-sublayout)))
                                                                (:radius encapsulated-sublayout))
                                                 :drag-forms))
                            (if (= 0 (count (:children (vedn/get-child (storage/project-attr :form)
                                                                       layout-path))))
                              (render-sublayouts (adjust-layout base-sublayout
                                                                (geom/scale-point sublayout
                                                                                  (/ (* constants/drop-form-radius-factor
                                                                                        (:radius sublayout))))
                                                                (* constants/drop-form-radius-factor
                                                                   (:radius sublayout)))
                                                 :drag-forms)
                              (let [adjusted-layout (adjust-layout base-sublayout
                                                                   (geom/scale-point placement-pos
                                                                                     (/ radius))
                                                                   radius)]
                                (graphics/circle (update adjusted-layout
                                                         :radius
                                                         (partial * constants/drop-form-outline-radius-factor))
                                                 (:background (storage/color-scheme))
                                                 :drag-forms)
                                (render-sublayouts adjusted-layout
                                                   :drag-forms))))))))))))
          (render-formbars)

          ;; Draw discard circle, icon, and last discarded form
          (graphics/circle (assoc (geom/add-points app-pos
                                                   (select-keys app-size [:y]))
                                  :radius constants/lower-corner-zone-radius)
                           (if (= mouse-zone :discard)
                             (:highlight (storage/color-scheme))
                             (:foreground (storage/color-scheme)))
                           :menu)
          (graphics/circle (assoc (geom/add-points app-pos
                                                   (select-keys app-size [:y]))
                                  :radius (* (- 1 constants/corner-zone-bar-thickness)
                                             constants/lower-corner-zone-radius))
                           (:background (storage/color-scheme))
                           :menu)
          (let [last-discard (first (storage/project-attr :discard-history))
                radius (/ (* (- 1 constants/corner-zone-bar-thickness)
                             constants/lower-corner-zone-radius)
                          (inc (Math/sqrt 2)))
                base-circle-pos (-> app-pos
                                    (update :y (partial + (- (:y app-size) radius)))
                                    (update :x (partial + radius)))]
            (if last-discard
              (render-sublayouts (form-layout last-discard
                                              (assoc base-circle-pos
                                                     :radius (* radius
                                                                constants/discard-zone-form-radius-factor)))
                                 :menu)
              (let [angle-offset (geom/scale-point (geom/angle-point (* 0.25 geom/PI))
                                                   (* radius
                                                      constants/discard-zone-icon-radius-factor))]
                (graphics/circle (assoc base-circle-pos
                                        :radius (* radius
                                                   constants/discard-zone-icon-radius-factor))
                                 (:foreground (storage/color-scheme))
                                 :menu)
                (graphics/circle (assoc base-circle-pos
                                        :radius (* radius
                                                   constants/discard-zone-icon-radius-factor
                                                   (- 1 constants/discard-zone-icon-thickness)))
                                 (:background (storage/color-scheme))
                                 :menu)
                (graphics/line (geom/add-points base-circle-pos
                                                angle-offset)
                               (geom/subtract-points base-circle-pos
                                                     angle-offset)
                               (* radius
                                  (* constants/discard-zone-icon-radius-factor
                                     constants/discard-zone-icon-thickness))
                               (:foreground (storage/color-scheme))
                               :menu))))

          ;; Draw eval circle, icon, and last evaluation result
          (let [eval-zone-radius (attr :eval-zone-radius)]
            (graphics/circle (assoc (geom/add-points app-pos app-size)
                                    :radius eval-zone-radius)
                             (if (= mouse-zone :eval)
                               (:highlight (storage/color-scheme))
                               (:foreground (storage/color-scheme)))
                             :menu)
            (graphics/circle (assoc (geom/add-points app-pos app-size)
                                    :radius (* (- 1 constants/corner-zone-bar-thickness)
                                               eval-zone-radius))
                             (:background (storage/color-scheme))
                             :menu)
            (let [last-eval-form (first (storage/project-attr :eval-results))
                  radius (/ (* (- 1 constants/corner-zone-bar-thickness)
                               (attr :eval-zone-radius))
                            (inc (Math/sqrt 2)))
                  base-circle-pos (geom/subtract-points (geom/add-points app-pos app-size)
                                                        (geom/scale-point geom/unit radius))]
              (if last-eval-form
                (if (= last-eval-form :error)
                  (let [base-offset (geom/scale-point geom/unit
                                                      (* (Math/sqrt 0.5)
                                                         constants/new-icon-size
                                                         radius))]
                    (doseq [offset [base-offset (update base-offset :x -)]]
                      (graphics/line (geom/add-points base-circle-pos
                                                      (geom/scale-point offset -1))
                                     (geom/add-points base-circle-pos
                                                      offset)
                                     (* radius constants/new-icon-width)
                                     (:highlight (storage/color-scheme))
                                     :menu)))
                  (render-sublayouts (form-layout last-eval-form
                                                  (assoc base-circle-pos
                                                         :radius (* radius
                                                                    constants/eval-zone-form-radius-factor)))
                                     :menu))
                (let [caret-offset (geom/scale-point
                                    (geom/angle-point (- constants/eval-zone-icon-angle
                                                         geom/PI))
                                    (* radius
                                       constants/eval-zone-caret-factor))]
                  (graphics/polyline (mapv #(update %
                                                    :x (partial +
                                                                (* radius
                                                                   constants/eval-zone-caret-offset)))
                                           [(geom/add-points base-circle-pos
                                                             caret-offset)
                                            base-circle-pos
                                            (geom/add-points base-circle-pos
                                                             (update caret-offset :y -))])
                                     (* radius constants/eval-zone-icon-thickness)
                                     (:foreground (storage/color-scheme))
                                     :menu)
                  (let [underscore-base (-> base-circle-pos
                                            (update :y (partial +
                                                                (- (:y caret-offset))
                                                                (* radius
                                                                   constants/eval-zone-underscore-y-offset)))
                                            (update :x (partial +
                                                                (* radius
                                                                   constants/eval-zone-underscore-x-offset))))]
                    (graphics/line underscore-base
                                   (update underscore-base
                                           :x
                                           (partial + (* radius constants/eval-zone-underscore-size)))
                                   (* radius constants/eval-zone-icon-thickness)
                                   (:foreground (storage/color-scheme))
                                   :menu))))))

          ;; Draw text-mode circle and icon
          (graphics/circle (assoc (geom/add-points app-pos
                                                   (select-keys app-size [:x]))
                                  :radius constants/upper-corner-zone-radius)
                           (if (= mouse-zone :text-icon)
                             (:highlight (storage/color-scheme))
                             (:foreground (storage/color-scheme)))
                           :menu)
          (let [radius (/ (* (- 1 constants/corner-zone-bar-thickness)
                             constants/upper-corner-zone-radius)
                          (inc (Math/sqrt 2)))
                base-circle-pos (geom/add-points (geom/add-points app-pos
                                                                  (select-keys app-size [:x]))
                                                 (update (geom/scale-point geom/unit radius)
                                                         :x -))]
            (graphics/text "txt"
                           base-circle-pos
                           radius
                           (:text (storage/color-scheme))
                           :menu))

          (when (and (= mouse-zone :formbar) current-placement-form)
            (let [formbar-path (formbar-path-at mouse)]
              (when formbar-path
                (let [arrangement (formbar-arrangement)
                      screen-side (first formbar-path)
                      bar-arrangement (get-in arrangement formbar-path)
                      form-spacing (* (storage/formbar-radius)
                                      (- 1 constants/formbar-outline-thickness))
                      insertion-index (get-formbar-insertion-index)
                      placement-circle
                      (if (zero? (count (:circles bar-arrangement)))
                        (-> bar-arrangement
                            (assoc :radius
                                   (* form-spacing
                                      constants/formbar-placement-size)))
                        (let [horizontal? (#{:top :bottom} screen-side)]
                          (-> bar-arrangement
                              (assoc :radius
                                     (* form-spacing
                                        constants/formbar-placement-size))
                              (update (if horizontal? :x :y)
                                      #(+ % (* (- insertion-index 0.5) 2 form-spacing)))
                              (update (if horizontal? :y :x)
                                      #((if (#{:top :left} screen-side) + -)
                                        %
                                        (* form-spacing
                                           constants/formbar-placement-offset))))))]
                  (render-sublayouts (form-layout (placement-form) placement-circle)
                                     :formbar)))))))

      :settings
      (let [{:keys [mouse]} @app-state]
        (render-formbars)
        (doseq [i (range constants/settings-pages)]
          (graphics/circle (settings-circle i)
                           (:foreground (storage/color-scheme))
                           :background))

        ;; Sliders
        (let [center-circle (settings-circle 1)
              center-radius (:radius center-circle)]
          (doseq [slider-index (range (count constants/settings-sliders))]
            (let [[slider-name slider-key] (nth constants/settings-sliders slider-index)
                  y (* center-radius
                       (+ constants/settings-top-slider-y
                          (* slider-index constants/settings-slider-spacing)))
                  left (geom/add-points center-circle
                                        {:x (* -1
                                               center-radius
                                               constants/settings-slider-width)
                                         :y y})
                  right (geom/add-points center-circle
                                         {:x (* center-radius
                                                constants/settings-slider-width)
                                          :y y})]
              (graphics/line left right
                             (* center-radius
                                constants/settings-slider-radius
                                2)
                             (:background (storage/color-scheme))
                             :background)
              (doseq [p [right left]]
                (graphics/circle (assoc p
                                        :radius (* center-radius
                                                   constants/settings-slider-radius))
                                 (:background (storage/color-scheme))
                                 :background))
              (graphics/circle (assoc (geom/tween-points left right (storage/attr slider-key))
                                      :radius (* center-radius
                                                 constants/settings-slider-radius
                                                 constants/settings-slider-inner-radius-factor))
                               (:foreground (storage/color-scheme))
                               :background)
              (graphics/text slider-name
                             (geom/add-points center-circle
                                              {:y (+ y (* center-radius constants/settings-slider-text-y))})
                             (* center-radius
                                constants/settings-slider-text-size
                                (count slider-name))
                             (:text (storage/color-scheme))
                             :background))))

        ;; Render project dropdown
        (let [center-circle (settings-circle 0)
              center-radius (:radius center-circle)
              bar-width (* 2 center-radius constants/settings-project-dropdown-width)
              bar-height (* center-radius constants/settings-project-dropdown-height)]
          (graphics/rect [(-> center-circle
                              (update :x #(+ (- % (* center-radius constants/settings-project-dropdown-width))
                                             (* bar-height
                                                constants/settings-project-dropdown-x-shrink-factor)))
                              (update :y (partial +
                                                  (* center-radius constants/settings-project-dropdown-y))))
                          {:x (- bar-width
                                 (* bar-height
                                    2
                                    constants/settings-project-dropdown-x-shrink-factor))
                           :y bar-height}]
                         (:highlight (storage/color-scheme))
                         :background)
          (graphics/circle (-> center-circle
                               (update :x #(+ (- % (* center-radius constants/settings-project-dropdown-width))
                                              (* bar-height
                                                 constants/settings-project-dropdown-x-shrink-factor)))
                               (update :y (partial +
                                                   (* center-radius constants/settings-project-dropdown-y)
                                                   (* 0.5 bar-height)))
                               (assoc :radius (* 0.5 bar-height)))
                           (:highlight (storage/color-scheme))
                           :background)
          (graphics/circle (-> center-circle
                               (update :x #(+ (- %
                                                 (* center-radius constants/settings-project-dropdown-width)
                                                 (* bar-height
                                                    constants/settings-project-dropdown-x-shrink-factor))
                                              bar-width))
                               (update :y (partial +
                                                   (* center-radius constants/settings-project-dropdown-y)
                                                   (* 0.5 bar-height)))
                               (assoc :radius (* 0.5 bar-height)))
                           (:highlight (storage/color-scheme))
                           :background)
          (graphics/text "Active Project"
                         (-> center-circle
                             (update :y (partial + (* center-radius (+ constants/settings-project-dropdown-y
                                                                       constants/settings-project-text-y)))))
                         (* center-radius constants/settings-project-text-size)
                         (:text (storage/color-scheme))
                         :background)
          (when (not (on-settings-page? 0))
            (graphics/text (storage/project-attr :name)
                           (-> center-circle
                               (update :y (partial + (* center-radius
                                                        (+ constants/settings-project-dropdown-y
                                                           (* 0.5
                                                              constants/settings-project-dropdown-height))))))
                           (* center-radius
                              constants/settings-project-name-size
                              (count (storage/project-attr :name)))
                           (:text (storage/color-scheme))
                           :background))

          (let [button-circles (settings-button-circles)]
            (doseq [index (range (count constants/settings-project-buttons))]
              (let [type (nth constants/settings-project-buttons index)
                    button-circle (nth button-circles index)
                    button-radius (:radius button-circle)]
                (graphics/circle button-circle
                                 (:background (storage/color-scheme))
                                 :background)
                (graphics/circle (update button-circle
                                         :radius
                                         (partial * constants/settings-project-button-inner-radius-factor))
                                 (if (= mouse-zone type)
                                   (:highlight (storage/color-scheme))
                                   (:foreground (storage/color-scheme)))
                                 :background)
                (case type
                  :rename-project
                  (let [eraser-offset (update (geom/scale-point geom/unit
                                                                (* button-radius
                                                                   (Math/sqrt 0.5)
                                                                   constants/settings-project-button-inner-radius-factor
                                                                   constants/rename-icon-size))
                                              :y -)
                        width (* button-radius
                                 constants/settings-project-button-inner-radius-factor
                                 constants/rename-icon-width)
                        line-width (* button-radius
                                      constants/settings-button-line-width)
                        eraser (geom/add-points button-circle
                                                eraser-offset)
                        eraser-top (geom/add-points eraser
                                                    (geom/scale-point geom/unit
                                                                      (* (Math/sqrt 0.5)
                                                                         -0.5
                                                                         width)))
                        eraser-bottom (geom/add-points eraser
                                                       (geom/scale-point geom/unit
                                                                         (* (Math/sqrt 0.5)
                                                                            0.5
                                                                            width)))
                        eraser-edge-top (geom/add-points eraser-top
                                                         (geom/scale-point eraser-offset
                                                                           (- constants/rename-icon-eraser-size)))
                        eraser-edge-bottom (geom/add-points eraser-bottom
                                                            (geom/scale-point eraser-offset
                                                                              (- constants/rename-icon-eraser-size)))
                        tip-top (geom/add-points eraser-top
                                                 (geom/scale-point eraser-offset
                                                                   (- constants/rename-icon-tip-size
                                                                      (inc constants/rename-icon-tip-factor))))
                        tip-bottom (geom/add-points eraser-bottom
                                                    (geom/scale-point eraser-offset
                                                                      (- constants/rename-icon-tip-size
                                                                         (inc constants/rename-icon-tip-factor))))
                        tip (geom/add-points button-circle
                                             (geom/scale-point eraser-offset
                                                               (- constants/rename-icon-tip-factor)))]
                    (graphics/polyline [tip
                                        tip-bottom
                                        eraser-bottom
                                        eraser-top
                                        tip-top
                                        tip]
                                       line-width
                                       (:background (storage/color-scheme))
                                       :background)
                    (graphics/line eraser-edge-top
                                   eraser-edge-bottom
                                   line-width
                                   (:background (storage/color-scheme))
                                   :background)
                    (graphics/line tip-top
                                   tip-bottom
                                   line-width
                                   (:background (storage/color-scheme))
                                   :background))

                  :new-project
                  (let [size (* button-radius
                                constants/settings-project-button-inner-radius-factor
                                constants/new-icon-size)
                        width (* button-radius
                                 constants/settings-project-button-inner-radius-factor
                                 constants/new-icon-width)]
                    (doseq [dim [:x :y]]
                      (graphics/line (update button-circle
                                             dim
                                             (partial + (- size)))
                                     (update button-circle
                                             dim
                                             (partial + size))
                                     width
                                     (:background (storage/color-scheme))
                                     :background)))

                  :duplicate-project
                  (let [width (* button-radius
                                 constants/duplicate-icon-width)
                        height (* button-radius
                                  constants/duplicate-icon-height)
                        line-width (* button-radius
                                      constants/settings-button-line-width)
                        corner {:x width
                                :y height}
                        base-offset (geom/scale-point geom/unit
                                                      (* button-radius
                                                         constants/duplicate-icon-offset))]
                    (doseq [offset [(geom/scale-point base-offset -1)
                                    base-offset]]
                      (let [base (geom/add-points button-circle offset)]
                        (graphics/rect [(geom/add-points base
                                                         (geom/scale-point corner -1))
                                        (geom/scale-point corner 2)]
                                       (if (= mouse-zone :duplicate-project)
                                         (:highlight (storage/color-scheme))
                                         (:foreground (storage/color-scheme)))
                                       :background)
                        (graphics/polyline (mapv (fn [dims]
                                                   (geom/add-points base
                                                                    (reduce #(update %1 %2 -)
                                                                            corner
                                                                            dims)))
                                                 [[]
                                                  [:x]
                                                  [:x :y]
                                                  [:y]
                                                  []])
                                           line-width
                                           (:background (storage/color-scheme))
                                           :background))))

                  :delete-project
                  (let [offset (geom/scale-point geom/unit
                                                 (* button-radius
                                                    (Math/sqrt 0.5)
                                                    constants/settings-project-button-inner-radius-factor
                                                    constants/new-icon-size))
                        width (* button-radius
                                 constants/settings-project-button-inner-radius-factor
                                 constants/new-icon-width)]
                    (doseq [offset-modifier [identity #(update % :y -)]]
                      (let [modified-offset (offset-modifier offset)]
                        (graphics/line (geom/add-points button-circle
                                                        (geom/scale-point modified-offset
                                                                          -1))
                                       (geom/add-points button-circle
                                                        modified-offset)
                                       width
                                       (:background (storage/color-scheme))
                                       :background)))))))))

        ;; Render scroll circle
        (let [scroll-circle (settings-bar-scroll-circle)
              scroll-direction (storage/attr :scroll-direction)
              triangle-base (geom/add-points scroll-circle
                                             (geom/scale-point scroll-direction
                                                               (* (:radius scroll-circle)
                                                                  constants/settings-bar-scroll-triangle-pos)))
              color (if (= mouse-zone :scroll-circle)
                      (:highlight (storage/color-scheme))
                      (:foreground (storage/color-scheme)))]
          (graphics/circle scroll-circle
                           color
                           :background)
          (graphics/circle (update scroll-circle
                                   :radius
                                   (partial * constants/settings-bar-scroll-circle-inner-radius))
                           (:background (storage/color-scheme))
                           :background)
          (graphics/polygon (mapv #(geom/add-points triangle-base
                                                    (geom/scale-point %
                                                                      (:radius scroll-circle)))
                                  [(geom/scale-point scroll-direction
                                                     constants/settings-bar-scroll-triangle-height)
                                   (geom/scale-point (assoc scroll-direction
                                                            :x (:y scroll-direction)
                                                            :y (- (:x scroll-direction)))
                                                     constants/settings-bar-scroll-triangle-width)
                                   (geom/scale-point (assoc scroll-direction
                                                            :x (- (:y scroll-direction))
                                                            :y (:x scroll-direction))
                                                     constants/settings-bar-scroll-triangle-width)])
                            color
                            :background))

        ;; Render color scheme page
        (let [center-circle (settings-circle 2)
              center-radius (:radius center-circle)]
          (graphics/text "Color Scheme"
                         (-> center-circle
                             (update :y
                                     (partial +
                                              (* center-radius
                                                 constants/settings-color-header-text-y))))
                         (* center-radius constants/settings-color-header-text-size)
                         (:text (storage/color-scheme))
                         :background)

          (doseq [i (range (count constants/color-schemes))]
            (let [color-scheme (nth constants/color-schemes i)
                  name (:name color-scheme)
                  y (* center-radius
                       (+ constants/settings-color-text-y
                          (* i constants/settings-color-spacing)))
                  display-colors (mapv #(% color-scheme)
                                       [:background :foreground :highlight])]
              (doseq [[color layer]
                      (if (= i (color-scheme-index-at mouse))
                        [[(:highlight color-scheme) 0]]
                        (mapv vector
                              display-colors
                              (range (count display-colors))))]
                (let [width (* 2
                               (- 1 (* layer constants/settings-color-width-factor))
                               constants/settings-color-width
                               center-radius)
                      height (* constants/settings-color-height
                                (- 1 (* layer constants/settings-color-height-factor))
                                center-radius)]
                  (graphics/rect [(-> center-circle
                                      (update :y #(- (+ % y)
                                                     (* 0.5
                                                        height)))
                                      (update :x #(- % (* 0.5 width))))
                                  {:x width
                                   :y height}]
                                 color
                                 :background)
                  (doseq [side [1 -1]]
                    (graphics/circle (-> center-circle
                                         (update :y (partial + y))
                                         (update :x (partial + (* 0.5
                                                                  side
                                                                  width)))
                                         (assoc :radius (* height 0.5)))
                                     color
                                     :background))))
              (graphics/text name
                             (-> center-circle
                                 (update :y
                                         (partial +
                                                  y)))
                             (* center-radius
                                constants/settings-color-text-size
                                (count name))
                             (:text color-scheme)
                             :background))))


        ;; Render new formbar circles
        (doseq [[new-formbar-circle] (new-formbar-circles)]
          (graphics/circle new-formbar-circle
                           (:highlight (storage/color-scheme))
                           :settings-overlay)
          (let [line-size (* (storage/formbar-radius)
                             constants/new-formbar-circle-radius
                             constants/new-icon-size)]
            (doseq [dim [:x :y]]
              (graphics/line (update new-formbar-circle dim (partial + line-size))
                             (update new-formbar-circle dim #(- % line-size))
                             (* (storage/formbar-radius)
                                constants/new-formbar-circle-radius
                                constants/new-icon-width)
                             (:background (storage/color-scheme))
                             :settings-overlay))))
        (let [formbar-path (formbar-path-at mouse)]
          (when formbar-path
            (let [current-formbar-arrangement (formbar-arrangement)
                  hovered-formbar (get-in current-formbar-arrangement formbar-path)
                  {:keys [width height]} hovered-formbar
                  center (geom/add-points hovered-formbar
                                          (geom/scale-point {:x width
                                                             :y height}
                                                            0.5))]
              (graphics/circle (assoc hovered-formbar
                                      :radius (storage/formbar-radius))
                               (:highlight (storage/color-scheme))
                               :settings-overlay)
              (when (pos? width)
                (graphics/circle (-> hovered-formbar
                                     (update :x (partial + width))
                                     (assoc :radius (storage/formbar-radius)))
                                 (:highlight (storage/color-scheme))
                                 :settings-overlay)
                (graphics/rect [(update hovered-formbar
                                        :y #(- % (storage/formbar-radius)))
                                {:x width
                                 :y (* 2 (storage/formbar-radius))}]
                               (:highlight (storage/color-scheme))
                               :settings-overlay))
              (when (pos? height)
                (graphics/circle (-> hovered-formbar
                                     (update :y (partial + height))
                                     (assoc :radius (storage/formbar-radius)))
                                 (:highlight (storage/color-scheme))
                                 :settings-overlay)
                (graphics/rect [(update hovered-formbar
                                        :x #(- % (storage/formbar-radius)))
                                {:x (* 2 (storage/formbar-radius))
                                 :y height}]
                               (:highlight (storage/color-scheme))
                               :settings-overlay))
              (let [offset (geom/scale-point geom/unit
                                             (* (Math/sqrt 0.5)
                                                constants/new-icon-size
                                                (storage/formbar-radius)))
                    upside-down-offset (update offset :y -)]
                (graphics/line (geom/add-points center
                                                offset)
                               (geom/add-points center
                                                (geom/scale-point offset -1))
                               (* (storage/formbar-radius) constants/new-icon-width)
                               (:background (storage/color-scheme))
                               :settings-overlay)
                (graphics/line (geom/add-points center
                                                upside-down-offset)
                               (geom/add-points center
                                                (geom/scale-point upside-down-offset -1))
                               (* (storage/formbar-radius) constants/new-icon-width)
                               (:background (storage/color-scheme))
                               :settings-overlay))))))

      :text
      (graphics/rect (let [[app-pos app-size] (graphics/app-rect)]
                       [(reduce #(update %1 %2 (partial + constants/text-page-border))
                                app-pos
                                [:x :y])
                        (reduce #(update %1 %2 (fn [v] (- v (* 2 constants/text-page-border))))
                                app-size
                                [:x :y])])
                     (:foreground (storage/color-scheme))
                     :background)

      nil)

    ;; Draw "settings" circle and icon, or "back" icon
    (let [radius (/ (* (- 1 constants/corner-zone-bar-thickness)
                       constants/upper-corner-zone-radius)
                    (inc (Math/sqrt 2)))
          base-circle-pos (geom/add-points app-pos
                                           (geom/scale-point geom/unit radius))
          text-page-valid? (attr :text-page-valid?)
          background-color (if (or (not text-page-valid?)
                                   (#{:settings-icon :back-icon} mouse-zone))
                             (:highlight (storage/color-scheme))
                             (:foreground (storage/color-scheme)))]
      (graphics/circle (assoc app-pos
                              :radius constants/upper-corner-zone-radius)
                       background-color
                       :menu)
      (if (= (attr :page) :code)
        (do (doseq [angle (map (partial * geom/TAU)
                               (u/prop-range constants/settings-zone-icon-spokes true))]
              (graphics/line base-circle-pos
                             (geom/add-points base-circle-pos
                                              (geom/scale-point (geom/angle-point angle)
                                                                (* radius
                                                                   constants/settings-zone-icon-spoke-length-factor)))
                             (* radius
                                constants/settings-zone-icon-spoke-width-factor)
                             (:text (storage/color-scheme))
                             :menu))
            (graphics/circle (assoc base-circle-pos
                                    :radius (* radius
                                               constants/settings-zone-icon-radius-factor))
                             (:text (storage/color-scheme))
                             :menu)
            (graphics/circle (assoc base-circle-pos
                                    :radius (* radius
                                               constants/settings-zone-icon-radius-factor
                                               constants/settings-zone-icon-inner-radius-factor))
                             background-color
                             :menu))
        (if text-page-valid?
          (let [tip (geom/add-points base-circle-pos
                                     {:x (- (* radius
                                               constants/back-icon-left-length-factor))})
                width (* radius constants/back-icon-width-factor)
                arrow-size (* radius constants/back-icon-tip-length-factor)]
            (graphics/line tip
                           (geom/add-points base-circle-pos
                                            {:x (* radius
                                                   constants/back-icon-right-length-factor)})
                           width
                           (:text (storage/color-scheme))
                           :menu)
            (graphics/polyline [(geom/add-points tip
                                                 {:x arrow-size
                                                  :y (- arrow-size)})
                                tip
                                (geom/add-points tip
                                                 {:x arrow-size
                                                  :y arrow-size})]
                               width
                               (:text (storage/color-scheme))
                               :menu))
          (let [base-offset (geom/scale-point geom/unit
                                              (* (Math/sqrt 0.5)
                                                 constants/new-icon-size
                                                 radius))]
            (doseq [offset [base-offset (update base-offset :x -)]]
              (graphics/line (geom/add-points base-circle-pos
                                              (geom/scale-point offset -1))
                             (geom/add-points base-circle-pos
                                              offset)
                             (* radius constants/new-icon-width)
                             (:text (storage/color-scheme))
                             :menu))))))))

(defn update-app []
  (let [delta (graphics/get-delta)
        {:keys [move zoom]} (storage/camera-speed (attr :camera-move-diff))]
    (when-not (zero? constants/scroll-speed)
      (storage/update-attr! :scroll-direction
                        #(geom/angle-point
                          (+ (geom/point-angle %)
                             (* constants/scroll-speed delta)))))
    (update-attr! :camera-pos
                  #(geom/tween-points (ideal-camera-pos)
                                      %
                                      (Math/pow move
                                                delta)))
    (update-attr! :camera-zoom
                  #(Math/pow Math/E
                             (u/tween (Math/log (ideal-camera-zoom))
                                      (Math/log %)
                                      (Math/pow zoom
                                                delta))))
    (update-attr! :eval-zone-radius
                  (fn [size]
                    (let [p (Math/pow constants/eval-zone-speed delta)]
                      (+ (* p size)
                         (* (- 1 p)
                            (if (= (get-mouse-zone) :eval)
                              constants/eval-zone-max-radius
                              constants/lower-corner-zone-radius))))))
    (let [{:keys [mouse page]} @app-state]
      (when (:down? mouse)
        (when (= (:down-zone mouse) :settings-slider)
          (let [settings-circle (settings-circle 1)
                x-off (apply - (map :x [mouse settings-circle]))
                adjusted-x-off (/ x-off (* (:radius settings-circle) constants/settings-slider-width))]
            (storage/set-attr! (second
                            (nth constants/settings-sliders
                                 (:down-settings-slider mouse)))
                           (min 1
                                (max 0
                                     (* 0.5
                                        (inc adjusted-x-off)))))))
        (when (= (:down-zone mouse) :scroll-circle)
          (storage/set-attr! :scroll-direction
                         (geom/angle-point
                          (let [raw-angle (mod (geom/point-angle
                                                (geom/subtract-points mouse
                                                                      (settings-circle 1)))
                                               geom/TAU)
                                snap-angle (some (fn [angle]
                                                   (when (< (min (Math/abs (- angle raw-angle))
                                                                 (- geom/TAU
                                                                    (Math/abs (- angle raw-angle))))
                                                            constants/scroll-angle-snap-distance)
                                                     angle))
                                                 (mapv (partial * geom/TAU)
                                                       (u/prop-range constants/scroll-angle-snap-positions true)))]
                            (or snap-angle
                                raw-angle)))))
        (when (and (mouse-dragging?)
                   (#{:code :settings} page)
                   (= (:down-zone mouse) :empty))
          (when (and (= page :code)
                     (> (count (attr :selected-layout-path))
                        1))
            (update-attr! :selected-layout-path
                          #(vec (take 1 %))))
          (update-attr! ({:code :ideal-scroll-pos
                          :settings :ideal-settings-scroll-pos}
                         page)
                        #(- %
                            (/ (geom/scalar-point-projection (geom/subtract-points mouse
                                                                                   (:last-pos mouse))
                                                             (storage/attr :scroll-direction))
                               (* (storage/base-zoom)
                                  constants/outer-form-spacing)))))))
    (update-attr! :ideal-scroll-pos
                  #(min (storage/project-form-count)
                        (max 0
                             %)))
    (update-attr! :scroll-pos
                  #(u/tween (attr :ideal-scroll-pos)
                            %
                            (Math/pow (:move (storage/camera-speed 0))
                                      delta)))
    (update-attr! :ideal-settings-scroll-pos
                  #(min (dec constants/settings-pages)
                        (max 0
                             %)))
    (update-attr! :settings-scroll-pos
                  #(u/tween (attr :ideal-settings-scroll-pos)
                            %
                            (Math/pow (:move (storage/camera-speed 0))
                                      delta)))
    (update-attr! :mouse
                  #(assoc %
                          :last-pos
                          (select-keys % [:x :y])))
    (resize-html-elements)
    (render-app-state)))

(defn log-eval-result [result]
  (storage/update-project-attr! :eval-results
                                #(conj %
                                       (first
                                        (:children
                                         (vedn/clj->vedn
                                          (if (string? result)
                                            (str \"
                                                 (string/escape result constants/char-escape-sting)
                                                 \")
                                            (str result))))))))

(defn log-eval-error [error]
  (js/console.error "Error during evaluation"
                    (ex-cause error))
  (storage/update-project-attr! :eval-results
                            #(conj %
                                   :error)))

(defn update-mouse-pos [event]
  (let [screen-pos (.-global (.-data event))
        x (.-x screen-pos)
        y (.-y screen-pos)
        width (graphics/app-width)
        height (graphics/app-height)
        size (graphics/app-size)
        current-pos {:x (/ (- x (* 0.5 (- width size))) size)
                     :y (/ (- y (* 0.5 (- height size))) size)}]
    (update-attr! :mouse
                  (fn [state]
                    (let [diff (geom/subtract-points state
                                                     current-pos)]
                      (assoc (merge state current-pos)
                             :drag-dist (when (:down? state)
                                          (+ (geom/point-magnitude diff)
                                             (:drag-dist state)))))))))



(defn remove-form [path]
  (storage/update-project-attr! :form
                            #(vedn/remove-child % path))
  (fill-empty-project))

(defn on-click-down [event]
  (update-mouse-pos event)
  (let [{:keys [mouse]} @app-state
        layout (adjusted-form-layouts)
        layout-path (layout-path-at layout mouse)
        zone (get-mouse-zone)
        settings-slider (settings-slider-at mouse)]
    (update-attr! :mouse
                  (fn [state]
                    (assoc state
                           :down? true
                           :down-path (vec layout-path)
                           :down-zone zone
                           :down-formbar-form-path (formbar-form-path-at mouse)
                           :down-settings-slider settings-slider)))
    (when (not= layout-path (attr :literal-text-input-path))
      (hide-literal-text-input))
    (when (= zone :settings-circle)
      (set-attr! :ideal-settings-scroll-pos
                 (settings-circle-at (attr :mouse))))))

(defn refresh-html-colors []
  (doseq [html-object (mapv attr
                            [:literal-text-input
                             :project-dropdown-input
                             :project-rename-input
                             :text-page-input])]
    (set! (.-color (.-style html-object))
          (graphics/html-color (:text (storage/color-scheme)))))
  (let [ss (first js/document.styleSheets)]
    (when ss
      (.insertRule ss
                   (str "::selection { background: "
                        (graphics/html-color (:background (storage/color-scheme)))
                        "}"))))
  (refresh-project-dropdown-input-names))

(defn on-click-up [event]
  (update-mouse-pos event)
  (let [{:keys [mouse selected-layout-path page]} @app-state
        layout (adjusted-form-layouts)
        layout-path (layout-path-at layout mouse)
        {:keys [down-zone down-path]} mouse]
    (if (mouse-dragging?)
      (case (get-mouse-zone)
        :program
        (let [insertion-path (layout-insertion-path-at layout mouse)
              current-placement-form (placement-form)]
          (when current-placement-form
            (storage/update-project-attr! :form
                                      (fn [form]
                                        (if (u/in? vedn/encapsulator-types
                                                   (:type (vedn/get-child form layout-path)))
                                          (vedn/replace-child form
                                                              (vec (concat layout-path '(0)))
                                                              current-placement-form)
                                          (if (= (count layout-path) (count insertion-path))
                                            (if (= :literal (:type (vedn/get-child form (vec insertion-path))))
                                              (vedn/replace-child form
                                                                  (vec layout-path)
                                                                  current-placement-form)
                                              (vedn/insert-child form
                                                                 (vec (concat layout-path '(0)))
                                                                 current-placement-form))
                                            (vedn/insert-child form
                                                               (vec insertion-path)
                                                               current-placement-form)))))))

        :discard
        (case down-zone
          :program
          (when down-path
            (if (empty? down-path)
              (do (storage/track-discard (first (:children (storage/project-attr :form))))
                  (storage/set-project-attr! :form {:type :vector :children [{:type :list :children []}]})
                  (set-attr! :selected-layout-path nil))
              (do (when (and (pos? (count selected-layout-path))
                             (= selected-layout-path down-path))
                    (update-attr! :selected-layout-path pop))
                  (storage/track-discard (vedn/get-child (storage/project-attr :form)
                                                     down-path))
                  (remove-form down-path))))

          :formbar
          (let [{:keys [down-formbar-form-path]} mouse]
            (when down-formbar-form-path
              (let [arrangement (formbar-arrangement)]
                (storage/track-discard
                 (get-in arrangement down-formbar-form-path))
                (storage/delete-project-formbar-form-at down-formbar-form-path))))

          nil)

        :eval
        (let [current-placement-form (placement-form)]
          (when current-placement-form
            (evaluation/eval-clj (vedn/vedn->clj current-placement-form)
                                 log-eval-result
                                 log-eval-error)))

        :formbar
        (let [current-placement-form (placement-form)]
          (when (and (= page :code)
                     (placement-form))
            (storage/add-project-formbar-form-at current-placement-form
                                             (formbar-path-at mouse)
                                             (get-formbar-insertion-index))))
        
        :empty
        (let [current-placement-form (placement-form)
              insertion-index (outer-form-insertion-index)]
          (when (and current-placement-form
                     insertion-index)
            (storage/update-project-attr! :form
                                      #(vedn/insert-child %
                                                          [insertion-index]
                                                          current-placement-form))))

        nil)
      (case (:down-zone mouse)
        :settings-icon
        (enter-page (case (attr :page)
                      :code :settings
                      :code))

        :back-icon
        (when (attr :text-page-valid?)
          (enter-page :code))

        :color-scheme
        (do (storage/set-attr! :color-scheme (color-scheme-index-at mouse))
            (refresh-html-colors))

        :text-icon
        (enter-page :text)

        :program
        (let [zoomed-form (vedn/get-child (storage/project-attr :form) down-path)
              {:keys [type value]} zoomed-form]
          (if (= type :literal)
            (activate-literal-text-input down-path)
            (do
              (set-attr! :ideal-scroll-pos (first down-path))
              (set-attr! :selected-layout-path down-path)))
          (when (not= down-path (attr :literal-text-input-path))
            (hide-literal-text-input)))

        :formbar
        (when (= (attr :page) :settings)
          (storage/delete-project-formbar-at (formbar-path-at mouse)))

        :new-formbar
        (storage/add-project-formbar-at (new-formbar-circle-path-at mouse))

        :empty
        (do (hide-literal-text-input)
            (set-attr! :selected-layout-path nil))

        :new-project
        (do (storage/new-project)
            (refresh-project-dropdown-input-names))

        :duplicate-project
        (do (storage/duplicate-project)
            (refresh-project-dropdown-input-names))

        :delete-project
        (do (storage/delete-project)
            (refresh-project-dropdown-input-names))

        :rename-project
        (activate-project-rename-input)

        nil))
    (update-attr! :mouse
                  (fn [state]
                    (assoc state
                           :down? false
                           :drag-dist 0)))
    (set-attr! :camera-move-diff (- (count layout-path)
                                    (count (attr :selected-layout-path))))))

(defn load-project []
  (storage/load-project (.-selectedIndex (:project-dropdown-input @app-state))))

(defn init []
  (set-attr! :camera-pos {:x 0 :y 0})
  (set-attr! :camera-zoom 1)
  (set-attr! :scroll-pos 0)
  (set-attr! :ideal-scroll-pos 0)
  (set-attr! :settings-scroll-pos 0)
  (set-attr! :settings-ideal-scroll-pos 0)
  (set-attr! :eval-zone-radius constants/lower-corner-zone-radius)
  (set-attr! :page :code)
  (set-attr! :text-page-valid? true)
  (let [literal-text-input (.createElement js/document "input")
        style (.-style literal-text-input)]
    (set! (.-type literal-text-input) "text")
    (.appendChild (.-body js/document) literal-text-input)
    (set-attr! :literal-text-input literal-text-input)
    (set! (.-position style) "absolute")
    (set! (.-textAlign style) "center")
    (set! (.-fontFamily style) constants/font-name)
    (set! (.-background style) "transparent")
    (set! (.-border style) "none")
    (set! (.-outline style) "none")
    (set! (.-display (.-style literal-text-input)) "none"))
  (let [project-dropdown-input (.createElement js/document "select")
        style (.-style project-dropdown-input)]
    (set! (.-onchange project-dropdown-input)
          #(do (load-project)
               (refresh-project-dropdown-input-names)))
    (.appendChild (.-body js/document) project-dropdown-input)
    (set-attr! :project-dropdown-input project-dropdown-input)
    (set! (.-textAlign style) "center")
    (set! (.-position style) "absolute")
    (set! (.-fontFamily style) constants/font-name)
    (set! (.-background style) "transparent")
    (set! (.-border style) "none")
    (set! (.-outline style) "none"))
  (let [project-rename-input (.createElement js/document "input")
        style (.-style project-rename-input)]
    (set! (.-onchange project-rename-input)
          #(do (storage/set-project-attr! :name (.-value project-rename-input))
               (refresh-project-dropdown-input-names)
               (hide-project-rename-input)))
    (.appendChild (.-body js/document) project-rename-input)
    (set-attr! :project-rename-input project-rename-input)
    (set! (.-textAlign style) "center")
    (set! (.-position style) "absolute")
    (set! (.-fontFamily style) constants/font-name)
    (set! (.-background style) "transparent")
    (set! (.-border style) "none")
    (set! (.-outline style) "none")
    (hide-project-rename-input))
  (let [text-page-input (.createElement js/document "textarea")
        style (.-style text-page-input)]
    (set! (.-type text-page-input) "text")
    (.appendChild (.-body js/document) text-page-input)
    (set-attr! :text-page-input text-page-input)
    (set! (.-onchange text-page-input)
          (fn [e]
            (try (let [current-text (.-value (attr :text-page-input))]
                   (storage/set-project-attr! :form (vedn/clj->vedn current-text))
                   (fill-empty-project)
                   (set-attr! :text-page-valid? true))
                 (catch :default _
                   (set-attr! :text-page-valid? false)))))
    (set! (.-resize style) "none")
    (set! (.-position style) "absolute")
    (set! (.-fontFamily style) constants/font-name)
    (set! (.-background style) "transparent")
    (set! (.-border style) "none")
    (set! (.-outline style) "none"))

  (refresh-html-colors)

  (enter-page :code)
  (resize-html-elements))