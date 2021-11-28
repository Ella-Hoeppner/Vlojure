(ns vlojure.formbar
  (:require [vlojure.graphics :refer [app-rect
                                      draw-circle
                                      draw-rect
                                      render-tool]]
            [vlojure.storage :refer [color-scheme
                                     update-global-attr!
                                     global-attr
                                     formbar-radius
                                     project-attr]]
            [vlojure.util :as u]
            [vlojure.geometry :as geom]
            [vlojure.constants :as c]
            [vlojure.layout :refer [render-sublayouts
                                    form-layout]]))

;;; This file defines functionality for rendering and interacting with
;;; formbars. Formbars are displayed on the sides of the screen when the user
;;; is on the code page. The user can drag forms to and from the formbar to
;;; edit their programs, and remove forms from the formbar by dragging them
;;; into the discard corner.

(defn formbar-offset [index]
  (+ (* (formbar-radius)
        (inc c/formbar-pos))
     (* index (* 2 (formbar-radius) c/formbar-spacing))))

(defn formbar-zone-size [side]
  (let [side-stage-count (count (get (project-attr :formbars) side))]
    (if (zero? side-stage-count)
      0
      (- (formbar-offset side-stage-count)
         (formbar-radius)))))

(defn formbar-arrangement []
  (zipmap
   c/screen-sides
   (map (fn [side]
          (let [[app-pos app-size] (app-rect)
                formbars (project-attr :formbars)
                horizontal? (#{:top :bottom} side)
                bar-stages (get formbars side)]
            (mapv (fn [stage stage-index]
                    (let [sizes (mapv (fn [bar]
                                        (case (:type bar)
                                          :tool 0
                                          :bindings (* (dec (:max-size bar))
                                                       2
                                                       (formbar-radius)
                                                       (- 1 c/formbar-outline-thickness))

                                          (max 0
                                               (let [bar-type (:type bar)
                                                     size (case bar-type
                                                            :tool 0
                                                            (count (:forms bar)))]
                                                 (* (dec size)
                                                    2
                                                    (formbar-radius)
                                                    (- 1 c/formbar-outline-thickness))))))
                                      stage)
                          total-size (+ (apply + sizes)
                                        (* (inc c/formbar-spacing)
                                           (formbar-radius)
                                           (dec (count stage))))
                          edge-offset (formbar-offset stage-index)
                          offsets (reduce (fn [offsets size]
                                            (conj offsets
                                                  (+ (last offsets)
                                                     size
                                                     (* (inc c/formbar-spacing)
                                                        (formbar-radius)))))
                                          [(* -0.5 total-size)]
                                          sizes)]
                      (mapv (fn [bar size bar-offset]
                              (let [bar-type (:type bar)
                                    bar-pos (case side
                                              :bottom {:x (+ (:x app-pos) (* 0.5 (:x app-size)) bar-offset)
                                                       :y (- (+ (:y app-pos) (:y app-size)) edge-offset)}
                                              :top {:x (+ (:x app-pos) (* 0.5 (:x app-size)) bar-offset)
                                                    :y (+ (:y app-pos) edge-offset)}
                                              :left {:x (+ (:x app-pos) edge-offset)
                                                     :y (+ (:y app-pos) (* 0.5 (:y app-size)) bar-offset)}
                                              :right {:x (- (+ (:x app-pos) (:x app-size)) edge-offset)
                                                      :y (+ (:y app-pos) (* 0.5 (:y app-size)) bar-offset)})
                                    radius (* (formbar-radius)
                                              (- 1 c/formbar-outline-thickness)
                                              c/formbar-form-size)]
                                (merge bar-pos
                                       {:width (if horizontal? size 0)
                                        :height (if horizontal? 0 size)
                                        :type bar-type
                                        :circles
                                        (case bar-type
                                          :tool [(assoc bar-pos
                                                        :radius radius)]
                                          :bindings (mapv (fn [name form-index]
                                                            (assoc (geom/add-points bar-pos
                                                                                    (geom/scale-point
                                                                                     {(if horizontal? :x :y)
                                                                                      (* 2
                                                                                         (- 1 c/formbar-outline-thickness)
                                                                                         (formbar-radius))}
                                                                                     form-index))
                                                                   :radius radius
                                                                   :form {:type :literal :value name}))
                                                          []
                                                          (range))
                                          (mapv (fn [form form-index]
                                                  (assoc (geom/add-points bar-pos
                                                                          (geom/scale-point
                                                                           {(if horizontal? :x :y)
                                                                            (* 2
                                                                               (- 1 c/formbar-outline-thickness)
                                                                               (formbar-radius))}
                                                                           form-index))
                                                         :radius radius
                                                         :form form))
                                                (:forms bar)
                                                (range)))}
                                       (when (= bar-type :tool)
                                         {:tool-type (:tool-type bar)}))))
                            stage
                            sizes
                            offsets)))
                  bar-stages
                  (range))))
        c/screen-sides)))

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
          c/screen-sides)))

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
                                                (formbar-radius))
                                            (<= (geom/point-magnitude
                                                 (geom/subtract-points pos
                                                                       (geom/add-points bar
                                                                                        (if horizontal?
                                                                                          {:x (:width bar)}
                                                                                          {:y (:height bar)}))))
                                                (formbar-radius))
                                            (geom/in-rect? (if horizontal?
                                                             [(geom/subtract-points bar
                                                                                    {:y (formbar-radius)})
                                                              {:x (:width bar)
                                                               :y (* 2 (formbar-radius))}]
                                                             [(geom/subtract-points bar
                                                                                    {:x (formbar-radius)})
                                                              {:x (* 2 (formbar-radius))
                                                               :y (:height bar)}])
                                                           pos))
                                    [side stage-index bar-index])))
                              (range (count stage)))))
                    (range (count arrangement)))))
          c/screen-sides)))

(defn formbar-insertion-path-at [pos]
  (let [arrangement (formbar-arrangement)
        layer-size (* 2
                      (formbar-radius)
                      c/formbar-spacing)
        potential-paths
        (filter identity
                (mapv (fn [side]
                        (let [[app-pos app-size] (app-rect)
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
                                                        (formbar-radius))))

                                              (>= insertion-index (count layer-bars))
                                              (let [bar (last layer-bars)]
                                                (max 0
                                                     (- (Math/abs (- perpendicular-pos
                                                                     (get bar perpendicular-dim)))
                                                        (+ (formbar-radius)
                                                           (max (:width bar)
                                                                (:height bar))))))

                                              :else 0)]
                              {:path [side
                                      rounded-layer-index
                                      insertion-index]
                               :error error}))))
                      c/screen-sides))
        min-error (apply min
                         (mapv :error potential-paths))]
    (some #(when (= min-error (:error %))
             (:path %))
          potential-paths)))

(defn formbar-insertion-circle [path]
  (let [[app-pos app-size] (app-rect)
        arrangement (formbar-arrangement)
        layer-path (take 2 path)
        [bar-side layer-index bar-index] path
        layer (get-in arrangement layer-path)
        layer-count (count layer)]
    (assoc (if (empty? layer)
             (geom/add-points app-pos
                              (let [outer-layer-spacing (* 2
                                                           (formbar-radius)
                                                           (+ 0.5 layer-index)
                                                           c/formbar-spacing)]
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
                   start-offset (* (formbar-radius)
                                   (inc c/formbar-spacing)
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
           :radius c/formbar-placement-circle-radius)))

(defn new-formbar-circles []
  (let [[app-pos app-size] (app-rect)
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
                                                        (* (formbar-radius)
                                                           (inc c/new-formbar-circle-radius)
                                                           c/formbar-spacing)))
                                            :radius (* c/new-formbar-circle-radius
                                                       (formbar-radius)))
                                     [side stage-index 0]
                                     (assoc (update (last stage)
                                                    dim
                                                    #(+ %
                                                        (size-attribute (last stage))
                                                        (* (formbar-radius)
                                                           (inc c/new-formbar-circle-radius)
                                                           c/formbar-spacing)))
                                            :radius (* c/new-formbar-circle-radius
                                                       (formbar-radius)))
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
                                                                   (- (+ (* c/new-formbar-circle-radius
                                                                            (formbar-radius))
                                                                         (formbar-offset (count arrangement-side)))
                                                                      (formbar-radius))))
                                :radius (* c/new-formbar-circle-radius
                                           (formbar-radius)))
                         [side (count arrangement-side) 0]])))
                   c/screen-sides))))

(defn new-formbar-circle-path-at [pos]
  (let [circles (new-formbar-circles)]
    (some (fn [[new-formbar-circle path]]
            (when (geom/in-circle? new-formbar-circle pos)
              path))
          circles)))

(defn saved-formbar-contents []
  (global-attr :saved-formbars))

(defn delete-saved-formbar! [index]
  (update-global-attr! :saved-formbars
                               #(u/vector-remove % index)))

(defn add-saved-formbar! [index formbar]
  (update-global-attr! :saved-formbars
                               (fn [saved-formbars]
                                 (if (>= index (count saved-formbars))
                                   (conj saved-formbars formbar)
                                   (u/vector-insert saved-formbars index formbar)))))

(defn render-formbars [mouse]
  (let [arrangement (formbar-arrangement)
        formbar-form-path (formbar-form-path-at mouse)]
    (doseq [side c/screen-sides]
      (let [horizontal? (#{:top :bottom} side)
            side-arrangement (get arrangement side)]
        (doseq [stage side-arrangement]
          (doseq [bar stage]
            (let [bar-type (:type bar)]
              (doseq [[color radius-factor]
                      [[(:foreground (color-scheme)) 1]
                       [(:background (color-scheme)) (- 1 c/formbar-outline-thickness)]]]
                (let [[primary-dim secondary-dim size-key]
                      (if horizontal?
                        [:x :y :width]
                        [:y :x :height])]
                  (when (not= bar-type :bindings)
                    (doseq [center [bar
                                    (geom/add-points bar
                                                     {primary-dim (size-key bar)})]]
                      (draw-circle (assoc center
                                          :radius (* (formbar-radius) radius-factor))
                                   color
                                   :formbar)))
                  (draw-rect (if (= bar-type :bindings)
                               [(geom/subtract-points bar
                                                      {primary-dim (* (formbar-radius) radius-factor)
                                                       secondary-dim (* (formbar-radius) radius-factor)})
                                {primary-dim (+ (size-key bar)
                                                (* 2 (formbar-radius) radius-factor))
                                 secondary-dim (* 2 (formbar-radius) radius-factor)}]
                               [(geom/subtract-points bar
                                                      {secondary-dim (* (formbar-radius) radius-factor)})
                                {primary-dim (size-key bar)
                                 secondary-dim (* 2 (formbar-radius) radius-factor)}])
                             color
                             :formbar))))))
        (when (and formbar-form-path
                   (= (first formbar-form-path) side))
          (draw-circle (update (get-in arrangement
                                       (butlast (formbar-form-path-at mouse)))
                               :radius (partial * (/ c/formbar-form-size)))
                       (:highlight (color-scheme))
                       :formbar))
        (doseq [stage side-arrangement]
          (doseq [bar stage]
            (if (= (:type bar) :tool)
              (do (draw-circle (first (:circles bar))
                               (:background (color-scheme))
                               :settings-overlay)
                  (render-tool (:tool-type bar)
                               (first (:circles bar))))
              (doseq [bar-circle (:circles bar)]
                (render-sublayouts (form-layout (:form bar-circle)
                                                              bar-circle)
                                          :formbar)))))))))
