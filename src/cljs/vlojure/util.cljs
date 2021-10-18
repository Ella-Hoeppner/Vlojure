(ns vlojure.util)

(defn in? [coll value]
  (some #{value} coll))

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

(defn clamp
  ([minimum maximum]
   (fn [x]
     (max minimum
          (min maximum
               x))))
  ([minimum maximum x]
   ((clamp minimum maximum)
    x))
  ([x]
   (clamp 0 1 x)))

(defn prop-range [n & [open]]
  (map #(/ %
           (if open
             n
             (dec n)))
       (range n)))

(defn log [& values]
  (apply js/console.log
         (map clj->js values))
  (last values))

(defn tween [a b t]
  (+ (* b t)
     (* a (- 1 t))))