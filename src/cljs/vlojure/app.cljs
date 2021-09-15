(ns vlojure.app
  (:require [clojure.string :as string]
            [vlojure.graphics :as graphics]
            [vlojure.storage :as storage]
            [vlojure.formbar :as formbar]
            [vlojure.layout :as layout]
            [vlojure.util :as u]
            [vlojure.geometry :as geom]
            [vlojure.constants :as constants]
            [vlojure.vedn :as vedn]
            [vlojure.evaluation :as evaluation]
            [vlojure.pages.text :as text-page]
            [vlojure.pages.settings :as settings-page]))

(def pages
  {:text text-page/page
   :settings settings-page/page})

(defn page-action [page action & args]
  (let [action (get-in pages [page action])]
    (when action
      (apply action args))))

(defn all-pages-action [action & args]
  (doseq [page (keys pages)]
    (apply page-action (conj args action page))))

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
  (let [last-page (attr :page)]
    (page-action last-page :exit))
  (page-action page :enter)
  (set-attr! :page page))

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

(defn current-form-layouts []
  (assoc geom/origin
         :sublayouts (mapv (fn [child index]
                             (layout/form-layout child
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

(defn resize-html-elements []
  (let [current-size (graphics/app-size)
        {:keys [page
                literal-text-input]} @app-state]
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
    (all-pages-action :resize-html)))

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

(defn get-mouse-zone []
  (let [[app-pos app-size] (graphics/app-rect)
        {:keys [mouse eval-zone-radius]} @app-state]
    (or (page-action (attr :page) :mouse-zone mouse)
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

            (formbar/formbar-path-at mouse)
            :formbar

            (reduce #(or %1
                         (geom/in-circle? %2 mouse))
                    false
                    (:sublayouts
                     (adjusted-form-layouts)))
            :program

            :else :empty)

          :empty))))

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
            (get-in (formbar/formbar-arrangement) down-formbar-form-path)))))

(defn get-formbar-insertion-index []
  (let [{:keys [mouse]} @app-state
        formbar-path (formbar/formbar-path-at mouse)]
    (when formbar-path
      (let [arrangement (formbar/formbar-arrangement)
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
          (layout/render-sublayouts (adjusted-form-layouts)
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
              (layout/render-sublayouts (layout/form-layout current-placement-form
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
                      (let [base-sublayout (layout/form-layout (placement-form)
                                                        (assoc geom/origin :radius 1))]
                        (if literal?
                          (layout/render-sublayouts (adjust-layout base-sublayout
                                                            (geom/scale-point sublayout
                                                                              (/ (:radius sublayout)))
                                                            (:radius sublayout))
                                             :drag-forms)
                          (if layout-encapsulated?
                            (let [encapsulated-sublayout (get-sublayout (adjusted-form-layouts) layout-path)]
                              (layout/render-sublayouts (adjust-layout base-sublayout
                                                                (geom/scale-point encapsulated-sublayout
                                                                                  (/ (:radius encapsulated-sublayout)))
                                                                (:radius encapsulated-sublayout))
                                                 :drag-forms))
                            (if (= 0 (count (:children (vedn/get-child (storage/project-attr :form)
                                                                       layout-path))))
                              (layout/render-sublayouts (adjust-layout base-sublayout
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
                                (layout/render-sublayouts adjusted-layout
                                                   :drag-forms))))))))))))
          (formbar/render-formbars mouse)

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
              (layout/render-sublayouts (layout/form-layout last-discard
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
                  (layout/render-sublayouts (layout/form-layout last-eval-form
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
            (let [formbar-path (formbar/formbar-path-at mouse)]
              (when formbar-path
                (let [arrangement (formbar/formbar-arrangement)
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
                  (layout/render-sublayouts (layout/form-layout (placement-form) placement-circle)
                                     :formbar)))))))

      nil)
    (page-action (attr :page)
                 :render
                 (attr :mouse) mouse-zone)

    ;; Draw "settings" circle and icon, or "back" icon
    (let [radius (/ (* (- 1 constants/corner-zone-bar-thickness)
                       constants/upper-corner-zone-radius)
                    (inc (Math/sqrt 2)))
          base-circle-pos (geom/add-points app-pos
                                           (geom/scale-point geom/unit radius))
          text-page-valid? (text-page/text-valid?)
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
          (let [settings-circle (settings-page/settings-circle 1)
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
                                                                      (settings-page/settings-circle 1)))
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
          (page-action page :scroll
                       (-
                        (/ (geom/scalar-point-projection (geom/subtract-points mouse
                                                                               (:last-pos mouse))
                                                         (storage/attr :scroll-direction))
                           (* (storage/base-zoom)
                              constants/outer-form-spacing))))
          (when (= page :code)
            (update-attr! :ideal-scroll-pos
                          #(- %
                              (/ (geom/scalar-point-projection (geom/subtract-points mouse
                                                                                     (:last-pos mouse))
                                                               (storage/attr :scroll-direction))
                                 (* (storage/base-zoom)
                                    constants/outer-form-spacing))))))))
    (update-attr! :ideal-scroll-pos
                  #(min (storage/project-form-count)
                        (max 0
                             %)))
    (update-attr! :scroll-pos
                  #(u/tween (attr :ideal-scroll-pos)
                            %
                            (Math/pow (:move (storage/camera-speed 0))
                                      delta)))
    (all-pages-action :update delta)
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
  (storage/fill-empty-project))

(defn on-click-down [event]
  (update-mouse-pos event)
  (let [{:keys [mouse]} @app-state
        layout (adjusted-form-layouts)
        layout-path (layout-path-at layout mouse)
        zone (get-mouse-zone)
        settings-slider (settings-page/settings-slider-at mouse)]
    (update-attr! :mouse
                  (fn [state]
                    (assoc state
                           :down? true
                           :down-path (vec layout-path)
                           :down-zone zone
                           :down-formbar-form-path (formbar/formbar-form-path-at mouse)
                           :down-settings-slider settings-slider)))
    (when (not= layout-path (attr :literal-text-input-path))
      (hide-literal-text-input))
    (when (= zone :settings-circle)
      (settings-page/set-ideal-scroll-pos! (settings-page/settings-circle-at (attr :mouse))))))

(defn refresh-html-colors []
  (doseq [html-object (mapv attr
                            [:literal-text-input])]
    (set! (.-color (.-style html-object))
          (graphics/html-color (:text (storage/color-scheme)))))
  (all-pages-action :refresh-html-colors)
  (let [ss (first js/document.styleSheets)]
    (when ss
      (.insertRule ss
                   (str "::selection { background: "
                        (graphics/html-color (:background (storage/color-scheme)))
                        "}")))))

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
              (let [arrangement (formbar/formbar-arrangement)]
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
                                             (formbar/formbar-path-at mouse)
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
        (when (text-page/text-valid?)
          (enter-page :code))

        :color-scheme
        (do (storage/set-attr! :color-scheme (settings-page/color-scheme-index-at mouse))
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
          (storage/delete-project-formbar-at (formbar/formbar-path-at mouse)))

        :new-formbar
        (storage/add-project-formbar-at (formbar/new-formbar-circle-path-at mouse))

        :empty
        (do (hide-literal-text-input)
            (set-attr! :selected-layout-path nil))

        :new-project
        (do (storage/new-project)
            (settings-page/refresh-dropdown-names))

        :duplicate-project
        (do (storage/duplicate-project)
            (settings-page/refresh-dropdown-names))

        :delete-project
        (do (storage/delete-project)
            (settings-page/refresh-dropdown-names))

        :rename-project
        (settings-page/activate-project-rename-input)

        nil))
    (update-attr! :mouse
                  (fn [state]
                    (assoc state
                           :down? false
                           :drag-dist 0)))
    (set-attr! :camera-move-diff (- (count layout-path)
                                    (count (attr :selected-layout-path))))))

(defn init []
  (set-attr! :camera-pos {:x 0 :y 0})
  (set-attr! :camera-zoom 1)
  (set-attr! :scroll-pos 0)
  (set-attr! :ideal-scroll-pos 0)
  (set-attr! :eval-zone-radius constants/lower-corner-zone-radius)
  (set-attr! :page :code)
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

  (doseq [action [:init :refresh-html-colors :resize-html]]
    (all-pages-action action))

  (refresh-html-colors)

  (enter-page :code)
  (resize-html-elements))