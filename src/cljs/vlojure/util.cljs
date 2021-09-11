(ns vlojure.util)

(defn in? [coll value]
  (some #{value} coll))

(defn seek [pred coll]
  (first (drop-while (complement pred) coll)))

(defn vector-insert [v index value]
  (if (empty? v)
    [value]
    (let [bounded-index (max 0 (min (dec count) index))]
      (vec (concat (subvec v 0 bounded-index)
                   (list value)
                   (subvec v bounded-index))))))

(defn vector-remove [v index]
  (vec (concat (subvec v 0 index)
               (subvec v (inc index)))))

(defn divis? [x div]
  (zero? (mod x div)))

(defn pad [coll value & [num]]
  (if num
    (vec (take num (concat coll (repeat value))))
    (concat coll (repeat value))))

(defn map-range [from-min from-max to-min to-max & [value]]
  (if value
    (+ (* (/ (- value from-min)
             (- from-max from-min))
          (- to-max to-min))
       to-min)
    (fn [value]
      (+ (* (/ (- value from-min)
               (- from-max from-min))
            (- to-max to-min))
         to-min))))

(defn prop-range [n & [open]]
  (map #(/ %
           (if open
             n
             (dec n)))
       (range n)))

(defn par [x]
  (prn x)
  x)

(defn sign [x]
  (cond (> x 0) 1
        (< x 0) -1
        :else 0))

(defn rand-geom [p]
  (if (> p (rand))
    0
    (inc (rand-geom p))))

(defn rand-exp []
  (- (Math/log (rand))))

(defn sigmoid [x]
  (/ 1 (inc (Math/exp (- x)))))

(defn swirly-looper [& [number freq-param amp-factor]]
  (let [number (or number 10)
        freqs (take number
                    (repeatedly
                     #(inc (rand-geom (or freq-param
                                          0.5)))))
        amplitudes (take number
                         (repeatedly
                          #(/ 1 (inc (* (rand-exp)
                                        (or amp-factor
                                            1))))))
        offsets (take number
                      (repeatedly
                       #(rand (* Math/PI 2))))]
    (fn [x]
      (/ (apply +
                (map (fn [f a o]
                       (* a
                          (Math/sin (+ o
                                       (* Math/PI 2
                                          f
                                          x)))))
                     freqs
                     amplitudes
                     offsets))
         (Math/sqrt number)))))

(defn update! [atom & update-args]
  (swap! atom
         (fn [a]
           (apply (partial update a)
                  update-args))))

(defn table-interp-function [table]
  (fn [x]
    (cond (< x 0) (first table)
          (>= x 1) (last table)
          :else
          (let [index (* x
                         (dec
                          (count table)))]
            (map-range 0 1
                       (nth table (int index)) (nth table (Math/ceil index))
                       (mod index 1))))))

(defn rand-exp-range [min-value max-value]
  (* min-value (Math/pow (/ max-value min-value)
                         (rand))))

(defn log [& values]
  (apply js/console.log
         (map clj->js values))
  (last values))

(defn tween [a b t]
  (+ (* b t)
     (* a (- 1 t))))