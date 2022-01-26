(ns vlojure.geometry
  (:require [vlojure.util :as u]))

(def PI Math/PI)
(def TAU (* 2 PI))

(def origin {:x 0 :y 0})
(def unit {:x 1 :y 1})

(def unit-square [origin unit])

(defn add-points [& points]
  (merge (first points)
         {:x (apply + (mapv :x points))
          :y (apply + (mapv :y points))}))

(defn multiply-points [& points]
  (merge (first points)
         {:x (apply * (mapv :x points))
          :y (apply * (mapv :y points))}))

(defn scale-point [a s]
  (let [scale-fn (partial * s)]
    (-> a
        (update :x scale-fn)
        (update :y scale-fn))))

(defn subtract-points [a b]
  (add-points a (scale-point b -1)))

(defn in-rect? [[pos size] point]
  (let [off (subtract-points point pos)]
    (and (<= 0 (:x off) (:x size))
         (<= 0 (:y off) (:y size)))))

(defn rect-within [[pos size] aspect-ratio]
  (let [rect-width (min (:x size) (* aspect-ratio (:y size)))
        rect-height (/ rect-width aspect-ratio)
        rect-size {:x rect-width :y rect-height}
        rect-pos (add-points pos
                             (scale-point (subtract-points size rect-size)
                                          0.5))]
    [rect-pos rect-size]))

(defn rect-center [[pos size]]
  (add-points pos
              (scale-point size 0.5)))

(defn circle-within [rect]
  (let [square (rect-within rect 1)]
    (assoc (rect-center square)
           :radius (/ (:x (second square)) 2))))

(defn rect-around [[pos size] aspect-ratio]
  (let [rect-width (max (:x size) (* aspect-ratio (:y size)))
        rect-height (/ rect-width aspect-ratio)
        rect-size {:x rect-width
                   :y rect-height}
        rect-pos (subtract-points pos
                                  (scale-point (subtract-points rect-size size)
                                               0.5))]
    [rect-pos rect-size]))

(defn point-square-magnitude [p]
  (+ (Math/pow (:x p) 2)
     (Math/pow (:y p) 2)))

(defn point-magnitude [p]
  (Math/sqrt (point-square-magnitude p)))

(defn normalize-point [p]
  (let [mag (point-magnitude p)]
    (if (zero? mag)
      p
      (scale-point p
                   (/ 1 mag)))))

(defn tween-points [p1 p2 t]
  (add-points (scale-point p2 t)
              (scale-point p1 (- 1 t))))

(defn average-point [points]
  (scale-point (add-points points)
               (/ 1 (count points))))

(defn angle-point [angle]
  {:x (Math/cos angle)
   :y (Math/sin angle)})

(defn point-angle [point]
  (Math/atan2 (:y point) (:x point)))

(defn reflect-point [point angle]
  (let [{:keys [x y]} point
        double-angle (* 2 (mod angle (* 2 Math/PI)))
        c (Math/cos double-angle)
        s (Math/sin double-angle)]
    {:x (- (* s y) (* c x))
     :y (+ (* s x) (* c y))}))

(defn normal-point [point]
  {:x (- (:y point))
   :y (:x point)})

(defn perfect-polygon [sides & [rotation]]
  (mapv (fn [i]
          (let [angle (+ rotation (/ (* TAU i) sides))]
            {:x (Math/cos angle)
             :y (Math/sin angle)}))
        (range sides)))

(defn map-point [from-pos from-size to-pos to-size point]
  (reduce (fn [pos dim]
            (assoc pos
                   dim
                   (let [from-min (dim from-pos)
                         from-max (+ from-min (dim from-size))
                         to-min (dim to-pos)
                         to-max (+ to-min (dim to-size))]
                     (u/map-range from-min
                                  from-max
                                  to-min
                                  to-max
                                  (dim point)))))
          {}
          [:x :y]))

(defn adjust-aspect-ratio [point ar]
  (multiply-points point
                   (last (rect-within unit-square ar))))

(defn unit-point [a]
  (scale-point a
               (point-magnitude a)))

(defn point-dot-product [a b]
  (+ (* (:x a) (:x b))
     (* (:y a) (:y b))))

(defn scalar-point-projection [a b]
  (point-dot-product a
                     (unit-point b)))

(defn scalar-point-rejection [a b]
  (/ (- (* (:y a) (:x b)) (* (:x a) (:y b)))
     (point-magnitude b)))

(defn in-circle? [circle pos]
  (< (point-square-magnitude (subtract-points circle pos))
     (Math/pow (:radius circle) 2)))

(defn rects-overlap? [[pos1 size1] [pos2 size2]]
  (let [l1 pos1
        l2 pos2
        r1 (add-points pos1 size1)
        r2 (add-points pos2 size2)]
    (not
     (or (>= (:x l1) (:x r2))
         (>= (:x l2) (:x r1))
         (<= (:y r1) (:y l2))
         (<= (:y r2) (:y l1))))))
