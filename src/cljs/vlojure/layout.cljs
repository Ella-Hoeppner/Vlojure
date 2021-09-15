(ns vlojure.layout
  (:require [vlojure.graphics :as graphics]
            [vlojure.storage :as storage]
            [vlojure.util :as u]
            [vlojure.geometry :as geom]
            [vlojure.constants :as constants]))

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

(defn render-sublayouts [layout & [layer]]
  (doseq [sublayout (flatten-layout layout)]
    (render-layout sublayout layer)))

