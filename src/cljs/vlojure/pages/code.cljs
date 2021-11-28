(ns vlojure.pages.code
  (:require [vlojure.graphics :refer [app-rect
                                      draw-circle
                                      draw-line
                                      draw-polyline
                                      draw-text
                                      text-size
                                      screen-x
                                      screen-y
                                      render-discard-zone
                                      in-discard-corner?
                                      html-color
                                      app-size]]
            [vlojure.storage :as storage]
            [vlojure.util :as u]
            [vlojure.formbar :as formbar]
            [vlojure.layout :as layout]
            [vlojure.geometry :as geom]
            [vlojure.constants :as c]
            [vlojure.vedn :as vedn]
            [vlojure.evaluation :as evaluation]
            [clojure.string :as string]
            [vlojure.app :as app]))

;;; This file contains the logic for the "code" page, which is the main page
;;; of the app. This is the page which loads when the player first opens
;;; Vlojure, and is the page in which they are intended to the majority of
;;; their code editing. This file defines the graphics and user interface
;;; for the code page.

(defonce literal-text-input (atom nil))
(defonce literal-text-input-path (atom nil))
(defonce ideal-scroll-pos (atom 0))
(defonce scroll-pos (atom nil))
(defonce eval-zone-radius (atom c/lower-corner-zone-radius))
(defonce camera-pos (atom geom/origin))
(defonce camera-zoom (atom 1))
(defonce camera-move-diff (atom nil))
(defonce selected-layout-path (atom nil))
(defonce down-path (atom nil))

(defn adjusted-form-circle []
  (geom/circle-within
   (app-rect)))

(defn zoomed-form-circle []
  (update (adjusted-form-circle)
          :radius
          (partial * (storage/base-zoom))))

(defn activate-literal-text-input [path]
  (let [text-input @literal-text-input]
    (set! (.-display (.-style text-input)) "block")
    (set! (.-value text-input)
          (:value (vedn/get-child (storage/project-attr :form)
                                  path)))
    (.focus text-input))
  (reset! literal-text-input-path path))

(defn hide-literal-text-input []
  (when @literal-text-input-path
    (storage/modify-code!
     (fn [form]
       (let [text-value (.-value @literal-text-input)
             value (if (pos? (count text-value))
                     text-value
                     "nil")
             new-forms (:children (vedn/clj->vedn value))]
         (reduce #(vedn/insert-child %1
                                     @literal-text-input-path
                                     %2)
                 (vedn/replace-child form
                                     @literal-text-input-path
                                     (first new-forms))
                 (reverse (rest new-forms))))))
    (let [text-input @literal-text-input]
      (set! (.-display (.-style text-input)) "none")
      (reset! literal-text-input-path nil))))

(defn current-form-layouts []
  (assoc geom/origin
         :sublayouts (mapv (fn [child index]
                             (layout/form-layout child
                                          (assoc (geom/scale-point (storage/attr :scroll-direction)
                                                                   (* c/outer-form-spacing
                                                                      index
                                                                      2))
                                                 :radius 1)))
                           (:children (storage/project-attr :form))
                           (range))
         :radius ##Inf))

(defn adjusted-form-layouts []
  (let [form-layouts (current-form-layouts)
        current-app-rect (app-rect)
        form-circle (adjusted-form-circle)]
    (reduce (fn [layouts index]
              (update-in layouts
                         [:sublayouts index]
                         #(-> %
                              (layout/adjust-layout @camera-pos
                                                    @camera-zoom)
                              (layout/adjust-layout geom/unit 0.5)
                              (layout/map-layout (geom/circle-within current-app-rect)
                                                 form-circle))))
            form-layouts
            (range (count (:sublayouts form-layouts))))))

(defn set-ideal-scroll-pos! [pos]
  (reset! ideal-scroll-pos pos))

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

(defn ideal-camera-pos []
  (geom/scale-point
   (let [scroll-dir (storage/attr :scroll-direction)
         total-layout (-> (current-form-layouts)
                          (layout/adjust-layout (geom/scale-point scroll-dir
                                                                  (* c/outer-form-spacing
                                                                     2
                                                                     @scroll-pos))
                                                1))
         sublayout (layout/get-sublayout total-layout
                                         @selected-layout-path)]
     (if (pos? (count @selected-layout-path))
       (geom/add-points sublayout
                        (geom/scale-point scroll-dir
                                          (* c/outer-form-spacing
                                             -2
                                             (first @selected-layout-path))))
       sublayout))
   -1))

(defn ideal-camera-zoom []
  (if @selected-layout-path
    (/ (storage/base-zoom)
       (:radius
        (layout/get-sublayout (current-form-layouts)
                              @selected-layout-path)))
    (storage/base-zoom)))

(defn placement-form [mouse]
  (let [{:keys [down? down-zone down-formbar-form-path]} mouse]
    (when down?
      (cond (= down-zone :program)
            (vedn/get-child (storage/project-attr :form)
                            @down-path)

            (= down-zone :eval)
            (let [last-result (first (storage/project-attr :eval-results))]
              (when (not= last-result :error)
                last-result))

            (= down-zone :discard)
            (first (storage/project-attr :discard-history))

            down-formbar-form-path
            (get-in (formbar/formbar-arrangement) down-formbar-form-path)))))

(defn dragged-tool [mouse]
  (let [{:keys [down? down-zone down-pos]} mouse]
    (when (and down?
               (= down-zone :formbar))
      (let [{:keys [tool-type]} (get-in (storage/project-attr :formbars)
                                        (formbar/formbar-path-at down-pos))]
        (when (c/draggable-tools tool-type)
          tool-type)))))

(defn apply-dragged-tool [tool path]
  (storage/modify-code!
   (fn [form]
     (let [child-form (vedn/get-child form path)]
       (if (or (#{:comment :quote-enclose :enclose :vector-enclose :fn-enclose :let-enclose} tool)
               (and (#{:literal-fn-replace} tool)
                    (= :list (:type child-form))))
         (vedn/replace-child form
                             path
                             (case tool
                               :comment {:type :comment :children [child-form]}
                               :quote-enclose {:type :quote :children [child-form]}
                               :enclose {:type :list :children [child-form]}
                               :vector-enclose {:type :vector :children [child-form]}
                               :literal-fn-replace {:type :lit-fn
                                                    :children (:children child-form)}
                               :fn-enclose {:type :list :children [{:type :literal :value "fn"}
                                                                   {:type :vector :children []}
                                                                   child-form]}
                               :let-enclose {:type :list :children [{:type :literal :value "let"}
                                                                    {:type :vector :children []}
                                                                    child-form]}))
         form)))))

(defn get-formbar-insertion-index [mouse]
  (let [formbar-path (formbar/formbar-path-at mouse)]
    (when formbar-path
      (let [arrangement (formbar/formbar-arrangement)
            screen-side (first formbar-path)
            bar-arrangement (get-in arrangement formbar-path)
            form-spacing (* (storage/formbar-radius)
                            (- 1 c/formbar-outline-thickness))]
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

(defn outer-form-insertion-index [mouse mouse-zone]
  (let [first-sublayout (first
                         (:sublayouts
                          (adjusted-form-layouts)))]
    (when (= mouse-zone :empty)
      (let [projection (geom/scalar-point-projection (geom/subtract-points mouse
                                                                           first-sublayout)
                                                     (storage/attr :scroll-direction))
            rejection (geom/scalar-point-rejection (geom/subtract-points mouse
                                                                         first-sublayout)
                                                   (storage/attr :scroll-direction))
            pos (/ projection
                   (* (storage/base-zoom)
                      c/outer-form-spacing))]
        (when (< (Math/abs rejection)
                 (* 0.5 (storage/base-zoom)))
          (if (neg? pos)
            -1
            (int pos)))))))

(defn set-camera-move-diff [val]
  (reset! camera-move-diff val))

(defn log-eval-result [result]
  (storage/update-project-attr! :eval-results
                                #(conj %
                                       (first
                                        (:children
                                         (vedn/clj->vedn
                                          (cond
                                            (string? result)
                                            (str \"
                                                 (string/escape result c/char-escape-string)
                                                 \")

                                            (fn? result)
                                            (apply str (rest (butlast (str [result]))))

                                            :else
                                            (str result))))))))

(defn log-eval-error [error]
  (js/console.error "Error during evaluation"
                    (ex-cause error))
  (storage/update-project-attr! :eval-results
                                #(conj %
                                       :error)))

(defn remove-form [path]
  (storage/modify-code! #(vedn/remove-child % path))
  (storage/fill-empty-project))

(defn init []
  (u/log "Code Page Initializing...")

  (let [literal-text-input-element (.createElement js/document "input")
        style (.-style literal-text-input-element)]
    (set! (.-type literal-text-input-element) "text")
    (.appendChild (.-body js/document) literal-text-input-element)
    (reset! literal-text-input literal-text-input-element)
    (set! (.-position style) "absolute")
    (set! (.-textAlign style) "center")
    (set! (.-fontFamily style) c/font-name)
    (set! (.-background style) "transparent")
    (set! (.-border style) "none")
    (set! (.-outline style) "none")
    (set! (.-display style) "none"))
  
  (app/register-page!
   :code
   {:enter
    (fn []
      (reset! scroll-pos @ideal-scroll-pos)
      (reset! camera-pos (ideal-camera-pos)))

    :exit
    (fn []
      (reset! scroll-pos nil))

    :resize-html
    (fn []
      (let [current-size (app-size)]
        (when literal-text-input
          (let [{:keys [x y radius]} (when @literal-text-input-path
                                       (layout/get-sublayout (adjusted-form-layouts)
                                                             @literal-text-input-path))
                text-input-str (.-value @literal-text-input)
                length (count text-input-str)
                new-text-size (* c/html-text-size-factor
                                 radius
                                 (text-size text-input-str))

                new-text-width (* new-text-size length)
                new-text-height (* c/html-text-height-factor new-text-size)
                style (.-style @literal-text-input)]
            (set! (.-left style)
                  (str (int (+ (- (screen-x x)
                                  (/ new-text-width 2))
                               (* c/html-text-x-offset new-text-size)))
                       "px"))
            (set! (.-top style)
                  (str (int (+ (- (screen-y y)
                                  (/ new-text-height 2))
                               (* c/html-text-y-offset new-text-size)))
                       "px"))
            (set! (.-width style)
                  (str new-text-width "px"))
            (set! (.-height style)
                  (str new-text-height "px"))
            (set! (.-fontSize style)
                  (str new-text-size
                       "px"))))))

    :refresh-html-colors
    (fn []
      (set! (.-color (.-style @literal-text-input))
            (html-color (:text (storage/color-scheme)))))

    :mouse-zone
    (fn [mouse]
      (let [[app-pos app-size] (app-rect)]
        (cond
          (<= (geom/point-magnitude
               (geom/subtract-points app-pos
                                     mouse))
              c/upper-corner-zone-radius)
          :settings-icon

          (<= (geom/point-magnitude
               (geom/subtract-points (geom/add-points app-pos
                                                      (select-keys app-size [:x]))
                                     mouse))
              c/upper-corner-zone-radius)
          :text-icon

          (in-discard-corner? mouse)
          :discard

          (<= (geom/point-magnitude
               (geom/subtract-points (geom/add-points app-pos
                                                      app-size)
                                     mouse))
              @eval-zone-radius)
          :eval

          (formbar/formbar-path-at mouse)
          :formbar

          (reduce #(or %1
                       (geom/in-circle? %2 mouse))
                  false
                  (:sublayouts
                   (adjusted-form-layouts)))
          :program

          :else :empty)))

    :render
    (fn [mouse mouse-zone]
      (when @literal-text-input-path
        (draw-circle (layout/get-sublayout (adjusted-form-layouts)
                                               @literal-text-input-path)
                         (:highlight (storage/color-scheme))
                         :menu))

      (let [{:keys [dragging?]} mouse
            [app-pos app-size] (app-rect)
            current-outer-form-insertion-index (outer-form-insertion-index mouse mouse-zone)
            current-placement-form (placement-form mouse)
            current-dragged-tool (dragged-tool mouse)]
        (layout/render-sublayouts (adjusted-form-layouts)
                                  :program)
        (when (and dragging?
                   (or current-dragged-tool
                       current-placement-form))
          (draw-circle (assoc mouse
                              :radius c/drag-cursor-radius)
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
                base-circle (select-keys (layout/expand-layout
                                          (layout/shift-layout adjusted-form-layout
                                                               (geom/scale-point scroll-direction
                                                                                 (* (if start? -1 1)
                                                                                    c/outer-form-spacing
                                                                                    radius)))
                                          c/drop-form-radius-factor)
                                         [:x :y :radius])]
            (draw-circle (update base-circle :radius (partial * c/drop-form-outline-radius-factor))
                         (:background (storage/color-scheme))
                         :drag)
            (layout/render-sublayouts (layout/form-layout current-placement-form
                                                          base-circle)
                                      :drag)))
        (let [layout-path (layout-path-at (adjusted-form-layouts)
                                          mouse)
              insertion-path (layout/layout-insertion-path-at (adjusted-form-layouts)
                                                              mouse)
              literal? (and (= (count layout-path) (count insertion-path))
                            (= :literal (:type (vedn/get-child (storage/project-attr :form)
                                                               insertion-path))))
              layout-encapsulated? (layout/layout-path-encapsulated? (adjusted-form-layouts)
                                                                     insertion-path)]
          (when (and layout-path (not dragging?) literal?)
            (draw-circle (layout/get-sublayout (adjusted-form-layouts)
                                               layout-path)
                         (:highlight (storage/color-scheme))
                         :program))
          (when (and layout-path current-placement-form)
            (let [sublayout (layout/get-sublayout (adjusted-form-layouts) layout-path)]
              (when dragging?
                (let [parent-radius (:radius sublayout)
                      radius (* c/drop-form-radius-factor
                                (:radius (first (:sublayouts sublayout))))
                      child-count (count (:sublayouts sublayout))
                      last-insertion-index (last insertion-path)
                      placement-pos (if (and (not literal?)
                                             layout-encapsulated?)
                                      (layout/get-sublayout (adjusted-form-layouts) layout-path)
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
                                                                              (* c/drop-form-offset-factor
                                                                                 radius))))))]
                  (when (= mouse-zone :program)
                    (draw-line mouse
                               placement-pos
                               c/drag-cursor-line-width
                               (:highlight (storage/color-scheme))
                               :program-overlay)
                    (let [base-sublayout (layout/form-layout (placement-form mouse)
                                                             (assoc geom/origin :radius 1))]
                      (if literal?
                        (layout/render-sublayouts (layout/adjust-layout base-sublayout
                                                                        (geom/scale-point sublayout
                                                                                          (/ (:radius sublayout)))
                                                                        (:radius sublayout))
                                                  :program-overlay)
                        (if layout-encapsulated?
                          (let [encapsulated-sublayout (layout/get-sublayout (adjusted-form-layouts) layout-path)]
                            (layout/render-sublayouts (layout/adjust-layout base-sublayout
                                                                            (geom/scale-point encapsulated-sublayout
                                                                                              (/ (:radius encapsulated-sublayout)))
                                                                            (:radius encapsulated-sublayout))
                                                      :program-overlay))
                          (if (= 0 (count (:children (vedn/get-child (storage/project-attr :form)
                                                                     layout-path))))
                            (layout/render-sublayouts (layout/adjust-layout base-sublayout
                                                                            (geom/scale-point sublayout
                                                                                              (/ (* c/drop-form-radius-factor
                                                                                                    (:radius sublayout))))
                                                                            (* c/drop-form-radius-factor
                                                                               (:radius sublayout)))
                                                      :program-overlay)
                            (let [adjusted-layout (layout/adjust-layout base-sublayout
                                                                        (geom/scale-point placement-pos
                                                                                          (/ radius))
                                                                        radius)]
                              (draw-circle (update adjusted-layout
                                                   :radius
                                                   (partial * c/drop-form-outline-radius-factor))
                                           (:background (storage/color-scheme))
                                           :program-overlay)
                              (layout/render-sublayouts adjusted-layout
                                                        :program-overlay))))))))))))
        (formbar/render-formbars mouse)

       ;; Draw discard circle, icon, and last discarded form
        (let [last-discard (first (storage/project-attr :discard-history))
              radius (/ (* (- 1 c/corner-zone-bar-thickness)
                           c/lower-corner-zone-radius)
                        (inc (Math/sqrt 2)))
              base-circle-pos (-> app-pos
                                  (update :y (partial + (- (:y app-size) radius)))
                                  (update :x (partial + radius)))]
          (render-discard-zone (= mouse-zone :discard)
                               last-discard)
          (when last-discard
            (layout/render-sublayouts (layout/form-layout last-discard
                                                          (assoc base-circle-pos
                                                                 :radius (* radius
                                                                            c/discard-zone-form-radius-factor)))
                                      :menu)))

       ;; Draw eval circle, icon, and last evaluation result
        (let []
          (draw-circle (assoc (geom/add-points app-pos app-size)
                              :radius @eval-zone-radius)
                       (if (= mouse-zone :eval)
                         (:highlight (storage/color-scheme))
                         (:foreground (storage/color-scheme)))
                       :menu)
          (draw-circle (assoc (geom/add-points app-pos app-size)
                              :radius (* (- 1 c/corner-zone-bar-thickness)
                                         @eval-zone-radius))
                       (:background (storage/color-scheme))
                       :menu)
          (let [last-eval-form (first (storage/project-attr :eval-results))
                radius (/ (* (- 1 c/corner-zone-bar-thickness)
                             @eval-zone-radius)
                          (inc (Math/sqrt 2)))
                base-circle-pos (geom/subtract-points (geom/add-points app-pos app-size)
                                                      (geom/scale-point geom/unit radius))]
            (if last-eval-form
              (if (= last-eval-form :error)
                (let [base-offset (geom/scale-point geom/unit
                                                    (* (Math/sqrt 0.5)
                                                       c/new-icon-size
                                                       radius))]
                  (doseq [offset [base-offset (update base-offset :x -)]]
                    (draw-line (geom/add-points base-circle-pos
                                                (geom/scale-point offset -1))
                               (geom/add-points base-circle-pos
                                                offset)
                               (* radius c/new-icon-width)
                               (:highlight (storage/color-scheme))
                               :menu)))
                (layout/render-sublayouts (layout/form-layout last-eval-form
                                                              (assoc base-circle-pos
                                                                     :radius (* radius
                                                                                c/eval-zone-form-radius-factor)))
                                          :menu))
              (let [caret-offset (geom/scale-point
                                  (geom/angle-point (- c/eval-zone-icon-angle
                                                       geom/PI))
                                  (* radius
                                     c/eval-zone-caret-factor))]
                (draw-polyline (mapv #(update %
                                              :x (partial +
                                                          (* radius
                                                             c/eval-zone-caret-offset)))
                                     [(geom/add-points base-circle-pos
                                                       caret-offset)
                                      base-circle-pos
                                      (geom/add-points base-circle-pos
                                                       (update caret-offset :y -))])
                               (* radius c/eval-zone-icon-thickness)
                               (:foreground (storage/color-scheme))
                               :menu)
                (let [underscore-base (-> base-circle-pos
                                          (update :y (partial +
                                                              (- (:y caret-offset))
                                                              (* radius
                                                                 c/eval-zone-underscore-y-offset)))
                                          (update :x (partial +
                                                              (* radius
                                                                 c/eval-zone-underscore-x-offset))))]
                  (draw-line underscore-base
                             (update underscore-base
                                     :x
                                     (partial + (* radius c/eval-zone-underscore-size)))
                             (* radius c/eval-zone-icon-thickness)
                             (:foreground (storage/color-scheme))
                             :menu))))))

       ;; Draw text-mode circle and icon
        (draw-circle (assoc (geom/add-points app-pos
                                             (select-keys app-size [:x]))
                            :radius c/upper-corner-zone-radius)
                     (if (= mouse-zone :text-icon)
                       (:highlight (storage/color-scheme))
                       (:foreground (storage/color-scheme)))
                     :menu)
        (let [radius (/ (* (- 1 c/corner-zone-bar-thickness)
                           c/upper-corner-zone-radius)
                        (inc (Math/sqrt 2)))
              base-circle-pos (geom/add-points (geom/add-points app-pos
                                                                (select-keys app-size [:x]))
                                               (update (geom/scale-point geom/unit radius)
                                                       :x -))]
          (draw-text "txt"
                     base-circle-pos
                     radius
                     (:text (storage/color-scheme))
                     :menu))

        (when (and (= mouse-zone :formbar) current-placement-form)
          (let [formbar-path (formbar/formbar-path-at mouse)]
            (when (and formbar-path
                       (not (:type (get-in (storage/project-attr :formbars)
                                           formbar-path))))
              (let [arrangement (formbar/formbar-arrangement)
                    screen-side (first formbar-path)
                    bar-arrangement (get-in arrangement formbar-path)
                    form-spacing (* (storage/formbar-radius)
                                    (- 1 c/formbar-outline-thickness))
                    insertion-index (get-formbar-insertion-index mouse)
                    placement-circle
                    (if (zero? (count (:circles bar-arrangement)))
                      (-> bar-arrangement
                          (assoc :radius
                                 (* form-spacing
                                    c/formbar-form-placement-size)))
                      (let [horizontal? (#{:top :bottom} screen-side)]
                        (-> bar-arrangement
                            (assoc :radius
                                   (* form-spacing
                                      c/formbar-form-placement-size))
                            (update (if horizontal? :x :y)
                                    #(+ % (* (- insertion-index 0.5) 2 form-spacing)))
                            (update (if horizontal? :y :x)
                                    #((if (#{:top :left} screen-side) + -)
                                      %
                                      (* form-spacing
                                         c/formbar-form-placement-offset))))))]
                (layout/render-sublayouts (layout/form-layout (placement-form mouse) placement-circle)
                                          :formbar)))))
        (app/render-top-left-button-background (= mouse-zone :settings-icon))
        (app/render-top-left-settings-button)))

    :update
    (fn [delta mouse mouse-zone]
      (swap! ideal-scroll-pos
             #(max 0
                   (min (count (:children (storage/project-attr :form)))
                        %)))
      (when @ideal-scroll-pos
        (swap! scroll-pos
               #(u/tween @ideal-scroll-pos
                         (or % 0)
                         (Math/pow (:move (storage/camera-speed 0))
                                   delta))))
      (let [{:keys [move zoom]}
            (storage/camera-speed @camera-move-diff)]
        (swap! camera-pos
               #(geom/tween-points (ideal-camera-pos)
                                   %
                                   (Math/pow move
                                             delta)))
        (swap! camera-zoom
               #(Math/pow Math/E
                          (u/tween (Math/log (ideal-camera-zoom))
                                   (Math/log %)
                                   (Math/pow zoom
                                             delta)))))
      (swap! eval-zone-radius
             (fn [size]
               (let [p (Math/pow c/eval-zone-speed delta)]
                 (+ (* p size)
                    (* (- 1 p)
                       (if (= mouse-zone :eval)
                         c/eval-zone-max-radius
                         c/lower-corner-zone-radius))))))
      (when (and (:dragging? mouse)
                 (= (:down-zone mouse) :empty)
                 (> (count @selected-layout-path) 1))
        (swap! selected-layout-path
               #(vec (take 1 %)))))

    :scroll
    (fn [diff]
      (swap! ideal-scroll-pos (partial + diff)))

    :click-down
    (fn [mouse mouse-zone]
      (reset! down-path
              (layout-path-at (adjusted-form-layouts) mouse))
      (when (not= (layout-path-at (adjusted-form-layouts)
                                  mouse)
                  (vec @literal-text-input-path))
        (hide-literal-text-input)))

    :click-up
    (fn [mouse mouse-zone]
      (let [layout (adjusted-form-layouts)
            layout-path (layout-path-at layout mouse)
            {:keys [down-zone]} mouse]
        (if (:dragging? mouse)
          (case mouse-zone
            :program
            (let [insertion-path (layout/layout-insertion-path-at layout mouse)
                  current-placement-form (placement-form mouse)
                  current-dragged-tool (dragged-tool mouse)]
              (when current-placement-form
                (storage/modify-code!
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
                                          current-placement-form))))))
              (when current-dragged-tool
                (apply-dragged-tool current-dragged-tool
                                    (layout-path-at layout mouse))))

            :discard
            (case down-zone
              :program
              (when @down-path
                (if (empty? @down-path)
                  (do (storage/track-discard (first (:children (storage/project-attr :form))))
                      (storage/set-project-attr! :form {:type :vector :children [{:type :list :children []}]})
                      (reset! selected-layout-path nil))
                  (do (when (and (pos? (count @selected-layout-path))
                                 (= @selected-layout-path @down-path))
                        (swap! selected-layout-path pop))
                      (storage/track-discard (vedn/get-child (storage/project-attr :form)
                                                             @down-path))
                      (remove-form @down-path))))

              :formbar
              (let [{:keys [down-formbar-form-path]} mouse]
                (when (and down-formbar-form-path
                           (not (:type (get-in (storage/project-attr :formbars)
                                               (formbar/formbar-path-at (:down-pos mouse))))))
                  (let [arrangement (formbar/formbar-arrangement)]
                    (storage/track-discard
                     (get-in arrangement down-formbar-form-path))
                    (storage/delete-project-formbar-form-at down-formbar-form-path))))

              nil)

            :eval
            (let [current-placement-form (placement-form mouse)]
              (when current-placement-form
                (evaluation/eval-clj (vedn/vedn->clj current-placement-form)
                                     log-eval-result
                                     log-eval-error)))

            :formbar
            (let [current-placement-form (placement-form mouse)]
              (when (placement-form mouse)
                (let [formbar-path (formbar/formbar-path-at mouse)
                      formbar (get-in (storage/project-attr :formbars)
                                      formbar-path)]
                  (when (not (:type formbar))
                    (storage/add-project-formbar-form-at current-placement-form
                                                         formbar-path
                                                         (get-formbar-insertion-index mouse))))))

            :empty
            (let [current-placement-form (placement-form mouse)
                  insertion-index (outer-form-insertion-index mouse mouse-zone)]
              (when (and current-placement-form
                         insertion-index)
                (storage/modify-code!
                 #(vedn/insert-child %
                                     [insertion-index]
                                     current-placement-form))))

            nil)
          (case (:down-zone mouse)
            :program
            (let [zoomed-form (vedn/get-child (storage/project-attr :form) @down-path)]
              (if (= (:type zoomed-form) :literal)
                (activate-literal-text-input @down-path)
                (do
                  (reset! ideal-scroll-pos (first @down-path))
                  (reset! selected-layout-path @down-path)))
              (when (not= @down-path @literal-text-input-path)
                (hide-literal-text-input)))

            :empty
            (do (hide-literal-text-input)
                (reset! selected-layout-path nil))
            
            :settings-icon
            (app/enter-page :settings)

            nil))
        (set-camera-move-diff (- (count layout-path)
                                 (count @selected-layout-path)))))}))