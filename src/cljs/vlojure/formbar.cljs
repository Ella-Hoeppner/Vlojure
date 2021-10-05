(ns vlojure.formbar
  (:require [vlojure.graphics :as graphics]
            [vlojure.storage :as storage]
            [vlojure.util :as u]
            [vlojure.geometry :as geom]
            [vlojure.constants :as constants]
            [vlojure.layout :as layout]))

;;; This file defines functionality for rendering and interacting with
;;; formbars. Formbars are displayed on the sides of the screen when the user
;;; is on the code page. The user can drag forms to and from the formbar to
;;; edit their programs, and remove forms from the formbar by dragging them
;;; into the discard corner.

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

(defn formbar-discard-path-at [pos]
  (let [full-arrangement (formbar-arrangement)]
    (some (fn [side]
            (let [arrangement (get full-arrangement side)
                  horizontal? (#{:top :bottom} side)]
              (some (fn [stage-index]
                      (let [stage (nth arrangement stage-index)]
                        (some (fn [bar-index]
                                (let [bar (nth stage bar-index)]
                                  (when (<= (geom/point-magnitude
                                             (geom/subtract-points pos
                                                                   (geom/add-points bar
                                                                                    (geom/scale-point
                                                                                     (if horizontal?
                                                                                       {:x (:width bar)}
                                                                                       {:y (:height bar)})
                                                                                     0.5))))
                                            (* (- 1 constants/formbar-outline-thickness)
                                               (storage/formbar-radius)))
                                    [side stage-index bar-index])))
                              (range (count stage)))))
                    (range (count arrangement)))))
          constants/screen-sides)))

(defn formbar-insertion-path-at [pos]
  (let [arrangement (formbar-arrangement)
        layer-size (* 2
                      (storage/formbar-radius)
                      constants/formbar-spacing)
        potential-paths
        (filter identity
                (mapv (fn [side]
                        (let [[app-pos app-size] (graphics/app-rect)
                              side-offset (case side
                                            :left (- (:x pos)
                                                     (:x app-pos))
                                            :right (- (+ (:x app-pos)
                                                         (:x app-size))
                                                      (:x pos))
                                            :top (- (:y pos)
                                                    (:y app-pos))
                                            :bottom (- (+ (:y app-pos)
                                                          (:y app-size))
                                                       (:y pos)))
                              layer-index (max 0
                                               (/ side-offset
                                                  layer-size))
                              rounded-layer-index (int layer-index)
                              side-layers (get arrangement side)
                              layer-count (count side-layers)]
                          (when (or (< rounded-layer-index layer-count)
                                    (and (= rounded-layer-index layer-count)
                                         (< (mod layer-index 1)
                                            0.5)))
                            (let [layer-bars (if (= rounded-layer-index layer-count)
                                               []
                                               (nth side-layers rounded-layer-index))
                                  bar-centers (mapv (fn [bar]
                                                      (geom/add-points bar
                                                                       (geom/scale-point
                                                                        {:x (:width bar)
                                                                         :y (:height bar)}
                                                                        0.5)))
                                                    layer-bars)
                                  perpendicular-dim (if (#{:top :bottom} side)
                                                      :x :y)
                                  perpendicular-pos (get pos perpendicular-dim)
                                  bar-positions (mapv #(get % perpendicular-dim)
                                                      bar-centers)
                                  insertion-index (if (empty? bar-centers)
                                                    0
                                                    (or (some #(when (< perpendicular-pos
                                                                        (nth bar-positions %))
                                                                 %)
                                                              (range (count bar-centers)))
                                                        (count bar-centers)))
                                  error (cond (empty? layer-bars)
                                              (Math/abs
                                               (- perpendicular-pos 0.5))

                                              (= insertion-index 0)
                                              (let [bar (first layer-bars)]
                                                (max 0
                                                     (- (Math/abs (- perpendicular-pos
                                                                     (get bar perpendicular-dim)))
                                                        (storage/formbar-radius))))

                                              (>= insertion-index (count layer-bars))
                                              (let [bar (last layer-bars)]
                                                (max 0
                                                     (- (Math/abs (- perpendicular-pos
                                                                     (get bar perpendicular-dim)))
                                                        (+ (storage/formbar-radius)
                                                           (max (:width bar)
                                                                (:height bar))))))

                                              :else 0)]
                              {:path [side
                                      rounded-layer-index
                                      insertion-index]
                               :error error}))))
                      constants/screen-sides))
        min-error (apply min
                         (mapv :error potential-paths))]
    (some #(when (= min-error (:error %))
             (:path %))
          potential-paths)))

(defn formbar-insertion-circle [path]
  (let [[app-pos app-size] (graphics/app-rect)
        arrangement (formbar-arrangement)
        layer-path (take 2 path)
        [bar-side layer-index bar-index] path
        layer (get-in arrangement layer-path)
        layer-count (count layer)]
    (assoc (if (empty? layer)
             (geom/add-points app-pos
                              (let [outer-layer-spacing (* 2
                                                           (storage/formbar-radius)
                                                           (+ 0.5 layer-index)
                                                           constants/formbar-spacing)]
                                (case bar-side
                                  :left (-> app-size
                                            (update :y (partial * 0.5))
                                            (assoc :x outer-layer-spacing))
                                  :right (-> app-size
                                             (update :y (partial * 0.5))
                                             (update :x
                                                     #(- % outer-layer-spacing)))
                                  :top (-> app-size
                                           (update :x (partial * 0.5))
                                           (assoc :y outer-layer-spacing))
                                  :bottom (-> app-size
                                              (update :x (partial * 0.5))
                                              (update :y
                                                      #(- % outer-layer-spacing))))))
             (let [vertical? (#{:left :right} bar-side)
                   start-offset (* (storage/formbar-radius)
                                   (inc constants/formbar-spacing)
                                   0.5)]
               (if (>= bar-index (count layer))
                 (let [bar (last layer)]
                   (geom/add-points (select-keys bar [:x :y])
                                    {:x (:width bar)
                                     :y (:height bar)}
                                    {(if vertical? :y :x)
                                     start-offset}))
                 (geom/add-points (select-keys (nth layer bar-index) [:x :y])
                                  {(if vertical? :y :x)
                                   (- start-offset)}))))
           :radius constants/formbar-placement-circle-radius)))

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

(defn render-formbars [mouse]
  (let [arrangement (formbar-arrangement)
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
              (layout/render-sublayouts (layout/form-layout (:form bar-circle)
                                                            bar-circle)
                                        :formbar))))))))