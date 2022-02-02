(ns vlojure.layout
  (:require [vlojure.util :as u]
            [vlojure.constants :as c]
            [vlojure.graphics :refer [app-rect
                                      draw-circle
                                      draw-line
                                      draw-polyline
                                      draw-polygon
                                      draw-text
                                      app-size
                                      draw-rect
                                      resize-form-renderer
                                      clear-form-icon-canvas!
                                      save-form-icon-image!
                                      form-icon-available?
                                      draw-form-icon]]
            [vlojure.storage :refer [color-scheme]]
            [vlojure.geometry :refer [rects-overlap?
                                      add-points
                                      subtract-points
                                      scale-point
                                      unit
                                      TAU
                                      angle-point
                                      point-angle
                                      PI
                                      perfect-polygon
                                      tween-points
                                      in-circle?
                                      unit-square
                                      rect-in-circle?]]
            [vlojure.vedn :refer [encapsulator-types]]))

;;; This file defines functionality for rendering and interacting with
;;; layouts. A "layout" is a structure that defines the size and location of
;;; the elements within a ClojureScript expression. Layouts are displayed in
;;; the body of the "code" page, and also within formbars.

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
                                      :radius (* c/sole-subform-shrink-factor
                                                 (:radius current-layout))))]
                 (let [raw-radius (Math/sin (/ Math/PI subform-count))
                       unscaled-radius (/ raw-radius (inc raw-radius))
                       radius (* unscaled-radius
                                 (- 1 c/bubble-thickness)
                                 (:radius current-layout))]
                   (mapv (fn [subform i]
                           (let [angle (- (* Math/PI -0.5)
                                          (/ (* Math/PI 2 i) subform-count))]
                             (form-layout subform
                                          (assoc
                                           (add-points
                                            current-layout
                                            (scale-point
                                             (angle-point angle)
                                             (- (* (:radius current-layout)
                                                   (- 1 c/bubble-thickness))
                                                radius)))
                                           :radius (* c/subform-shrink-factor
                                                      radius)))))
                         children
                         (range)))))))))

(defn layout->form [{:keys [sublayouts]
                     :as layout}]
  (let [stripped-layout (select-keys layout [:type :value])]
    (if sublayouts
      (assoc stripped-layout
             :children
             (mapv layout->form sublayouts))
      stripped-layout)))

(defn should-render-layout? [{:keys [x y radius] :as circle}]
  (and (rects-overlap? (app-rect)
                       [{:x (- x radius) :y (- y radius)}
                        {:x (* 2 radius) :y (* 2 radius)}])
       (not (rect-in-circle? (app-rect)
                             (update circle
                                     :radius
                                     * (- 1 c/bubble-thickness))))))

(defn render-layout [layout & [layer]]
  (when (should-render-layout? layout)
    (let [center layout
          radius (:radius layout)]
      (when (#{:list :map :set :lit-fn :literal} (:type layout))
        (draw-circle layout
                     (:foreground (color-scheme))
                     layer))
      (when (#{:map :set} (:type layout))
        (let [r (:radius layout)]
          (doseq [base-angle [(* PI 0.25)
                              (* PI 0.75)
                              (* PI 1.25)
                              (* PI 1.75)]]
            (draw-polygon
             [(add-points layout
                          (scale-point (angle-point (- base-angle
                                                       c/map-point-width))
                                       r))
              (add-points layout
                          (scale-point (angle-point base-angle)
                                       (* (inc c/map-point-height) r)))
              (add-points layout
                          (scale-point (angle-point (+ base-angle
                                                       c/map-point-width))
                                       r))]
             (:foreground (color-scheme))
             layer))))
      (when (#{:set :lit-fn} (:type layout))
        (let [r (:radius layout)]
          (doseq [angle [0
                         (* PI 0.5)
                         PI
                         (* PI 1.5)]]
            (let [base-offset (scale-point (angle-point (+ angle (* PI 0.5)))
                                           (* r c/set-line-offset))]
              (doseq [offset [base-offset (scale-point base-offset -1)]]
                (let [origin (add-points offset layout)]
                  (draw-line origin
                             (add-points origin
                                         (scale-point
                                          (angle-point angle)
                                          (* (inc c/set-line-length) r)))
                             (* r c/set-line-width)
                             (:foreground (color-scheme))
                             layer)))))))
      (when (#{:list :map :set :lit-fn} (:type layout))
        (draw-circle (update layout
                             :radius (partial *
                                              (- 1 c/bubble-thickness)))
                     (:background (color-scheme))
                     layer))
      (when (= (:type layout) :vector)
        (draw-polygon (mapv #(add-points
                              center
                              (scale-point %
                                           (* radius
                                              c/vector-size-factor)))
                            (perfect-polygon 8
                                             (* PI 0.125)))
                      (:foreground (color-scheme))
                      layer)
        (draw-polygon (mapv #(add-points
                              center
                              (scale-point %
                                           (* radius
                                              c/vector-size-factor
                                              (- 1 c/bubble-thickness))))
                            (perfect-polygon 8
                                             (* PI 0.125)))
                      (:background (color-scheme))
                      layer))
      (when (= (:type layout) :quote)
        (let [radius (:radius layout)]
          (doseq [angle (mapv (partial * TAU)
                              (u/prop-range c/quote-divs true))]
            (let [[start end]
                  (map #(add-points layout
                                    (scale-point
                                     (angle-point
                                      (+ angle
                                         (* % (/ TAU c/quote-divs 4))))
                                     radius))
                       [-1 1])]
              (draw-line start end
                         (* radius
                            c/bubble-thickness)
                         (:foreground (color-scheme))
                         layer)))))
      (when (= (:type layout) :deref)
        (let [radius (:radius layout)]
          (doseq [angle (mapv (partial * TAU)
                              (u/prop-range c/deref-circles true))]
            (draw-circle (assoc (add-points layout
                                            (scale-point (angle-point angle)
                                                         radius))
                                :radius
                                (* c/deref-circle-size-factor
                                   (:radius layout)))
                         (:foreground (color-scheme))
                         layer))))
      (when (= (:type layout) :syntax-quote)
        (let [radius (:radius layout)]
          (doseq [angle (mapv (partial * TAU)
                              (u/prop-range c/syntax-quote-divs true))]
            (let [[start end]
                  (map #(add-points
                         layout
                         (scale-point
                          (angle-point
                           (+ angle
                              (* % (/ TAU c/syntax-quote-divs 4))))
                          (* radius
                             (+ 1
                                (* % c/syntax-quote-offset-factor)))))
                       [-1 1])]
              (draw-line start end
                         (* radius
                            c/bubble-thickness)
                         (:foreground (color-scheme))
                         layer)))))
      (when (= (:type layout) :comment)
        (let [radius (:radius layout)]
          (doseq [angle (mapv (partial * TAU)
                              (u/prop-range c/comment-divs true))]
            (draw-line
             (add-points layout
                         (scale-point (angle-point angle)
                                      radius))
             (add-points layout
                         (scale-point (angle-point angle)
                                      (* radius
                                         (- 1 c/comment-length-factor))))
             (* radius
                c/bubble-thickness)
             (:foreground (color-scheme))
             layer))))
      (when (= (:type layout) :unquote)
        (let [segment-angle (/ TAU 8)
              angles (mapv #(+ (* segment-angle %)
                               (/ TAU 16))
                           (range 8))
              points (mapv #(scale-point (angle-point %)
                                         (* radius
                                            c/vector-size-factor))
                           angles)]
          (doseq [[start-point end-point]
                  (partition 2 1 (conj points (first points)))]
            (let [div-size (/ 0.5 c/unquote-divs)
                  tween-starts (mapv #(* div-size (+ 0.5 (* 2 %)))
                                     (range c/unquote-divs))]
              (doseq [tween-start tween-starts]
                (draw-line (add-points layout
                                       (tween-points start-point
                                                     end-point
                                                     tween-start))
                           (add-points layout
                                       (tween-points start-point
                                                     end-point
                                                     (+ tween-start div-size)))
                           (* radius
                              c/bubble-thickness)
                           (:foreground (color-scheme))
                           layer))))))
      (when (= (:type layout) :unquote-splice)
        (let [segment-angle (/ TAU 8)
              angles (mapv #(+ (* segment-angle %)
                               (/ TAU 16))
                           (range 8))
              points (mapv #(scale-point (angle-point %)
                                         (* radius
                                            c/vector-size-factor))
                           angles)]
          (doseq [[start-point end-point]
                  (partition 2 1 (conj points (first points)))]
            (let [div-spacing (/ 0.5 c/unquote-splice-circles)]
              (doseq [t (u/prop-range c/unquote-splice-circles true)]
                (draw-circle (assoc (add-points layout
                                                (tween-points start-point
                                                              end-point
                                                              t))
                                    :radius
                                    (* c/deref-circle-size-factor
                                       (:radius layout)))
                             (:foreground (color-scheme))
                             layer))))))
      (when (= (:type layout) :meta)
        (let [radius (:radius layout)
              angle-offset (/ PI 2 c/meta-divs)]
          (doseq [angle (mapv (partial * TAU) (u/prop-range c/meta-divs true))]
            (let [tip (add-points layout
                                  (scale-point (angle-point angle)
                                               radius))
                  [start end]
                  (mapv #(add-points
                          layout
                          (scale-point (angle-point
                                        (+ angle (* % angle-offset)))
                                       (* radius (- 1 c/meta-length-factor))))
                        [1 -1])]
              (draw-polyline [start tip end]
                             (* radius
                                c/bubble-thickness)
                             (:foreground (color-scheme))
                             layer)))))
      (when (= (:type layout) :var-quote)
        (let [radius (:radius layout)]
          (doseq [angle (mapv (partial * TAU)
                              (u/prop-range c/var-quote-divs true))]
            (let [[start end]
                  (map #(add-points
                         layout
                         (scale-point (angle-point
                                       (+ angle
                                          (* % (/ TAU c/var-quote-divs 4))))
                                      radius))
                       [-1 1])]
              (draw-line start
                         end
                         (* radius
                            c/bubble-thickness)
                         (:foreground (color-scheme))
                         layer))
            (let [[start end]
                  (map #(add-points layout
                                    (scale-point (angle-point angle)
                                                 (* radius %)))
                       [1 (inc c/var-quote-length)])]
              (draw-line start
                         end
                         (* radius
                            c/bubble-thickness)
                         (:foreground (color-scheme))
                         layer)))))
      (when (= (:type layout) :literal)
        (draw-text (:value layout)
                   layout
                   (:radius layout)
                   (:text (color-scheme))
                   layer)))))

(defn render-total-layout-without-icon [layout & [layer]]
  (render-layout layout layer)
  (doseq [sublayout (:sublayouts layout)]
    (render-total-layout-without-icon sublayout
                                      layer)))

(defn create-form-icon! [form size]
  (when (not (form-icon-available? form size))
    (let [adjusted-size (* size (app-size))
          overflow-size (Math/ceil (* adjusted-size
                                      c/form-icon-canvas-overflow-factor))]
      (clear-form-icon-canvas!)
      (resize-form-renderer overflow-size)
      #_(let [border 0.025]
        (draw-rect [{:x 0 :y 0} {:x 1 :y border}]
                   0xff0000
                   :form-icon)
        (draw-rect [{:x 0 :y 0} {:x border :y 1}]
                   0xff0000
                   :form-icon)
        (draw-rect [{:x 0 :y (- 1 border)} {:x 1 :y border}]
                   0xff0000
                   :form-icon)
        (draw-rect [{:x (- 1 border) :y 0} {:x border :y 1}]
                   0xff0000
                   :form-icon))
      (render-total-layout-without-icon
       (form-layout form {:x 0.5
                          :y 0.5
                          :radius (* 0.5
                                     (/ adjusted-size
                                        overflow-size))})
       :form-icon)
      (save-form-icon-image! form))))

(defn render-total-layout [{:keys [radius] :as layout} & [layer]]
  (let [form (layout->form layout)]
    (if (and (not= layer :form-icon)
             (form-icon-available? form (* 2 radius)))
      (draw-form-icon form layout layer)
      (if (< radius 0.5)
        (do (create-form-icon! form
                               1)
            (draw-form-icon form layout layer))
        (do (render-layout layout layer)
            (doseq [sublayout (:sublayouts layout)]
              (render-total-layout sublayout
                                   layer)))))))

(defn shift-layout [layout offset]
  (-> layout
      (add-points offset)
      (update :sublayouts
              (fn [sublayouts]
                (mapv #(shift-layout % offset)
                      sublayouts)))))

(defn expand-layout [layout radius-factor]
  ((fn f [inner-layout]
     (-> inner-layout
         (update :radius
                 (partial * radius-factor))
         (merge (select-keys
                 (add-points layout
                             (scale-point (subtract-points inner-layout
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
   (subtract-points (subtract-points to (scale-point unit (:radius to)))
                    (subtract-points from (scale-point unit (:radius from))))
   (/ (:radius to) (:radius from))))

(defn get-sublayout [layout path]
  (if (empty? path)
    layout
    (get-sublayout (nth (:sublayouts layout)
                        (first path))
                   (rest path))))

(defn layout-path-encapsulated? [layout path]
  (boolean
   (u/in? encapsulator-types
          (:type (get-sublayout layout (butlast path))))))

(defn layout-insertion-path-at [layout pos]
  (when (in-circle? layout pos)
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
                  mouse-angle (mod (point-angle (subtract-points pos layout))
                                   TAU)
                  upper-angle (* TAU 0.75)
                  angle-offset (- mouse-angle upper-angle)]
              (if (< (Math/abs (- mouse-angle upper-angle))
                     (/ TAU (* 3 sub-count)))
                (list -1)
                (list (int (/ (* sub-count
                                 (mod (- angle-offset)
                                      TAU))
                              TAU))))))))))
