(ns vlojure.pages.settings
  (:require [vlojure.graphics :refer [app-rect
                                      draw-circle
                                      draw-line
                                      draw-polyline
                                      draw-polygon
                                      draw-rect
                                      draw-text
                                      render-tool
                                      screen-x
                                      screen-y
                                      render-discard-zone
                                      in-discard-corner?
                                      html-color
                                      app-size]]
            [vlojure.storage :refer [color-scheme
                                     add-project-formbar-form-at
                                     load-project
                                     formbar-radius
                                     set-global-attr!
                                     set-project-attr!
                                     delete-project-formbar-at
                                     add-project-formbar-at
                                     new-project
                                     duplicate-project
                                     delete-project
                                     camera-speed
                                     global-attr
                                     base-zoom
                                     project-attr]]
            [vlojure.util :as u]
            [vlojure.layout :refer [render-sublayouts
                                    form-layout]]
            [vlojure.formbar :refer [new-formbar-circle-path-at
                                     saved-formbar-contents
                                     render-formbars
                                     formbar-insertion-path-at
                                     formbar-insertion-circle
                                     new-formbar-circles
                                     formbar-path-at
                                     formbar-arrangement
                                     delete-saved-formbar!
                                     add-saved-formbar!]]
            [vlojure.geometry :refer [add-points
                                      subtract-points
                                      scale-point
                                      unit
                                      TAU
                                      angle-point
                                      point-angle
                                      tween-points
                                      in-circle?
                                      in-rect?
                                      circle-within
                                      point-magnitude]]
            [vlojure.constants :as c]
            [vlojure.app :refer [enter-page
                                 register-page!
                                 render-top-left-button-background
                                 render-top-left-back-button]]))

;;; This file contains the logic for the "settings" page. This page lets the
;;; user create and delete projects as well as renaming and switching between
;;; them, adjust the color scheme and different aspects of the app's visual
;;; appearance, and add or remove formbars for the current project.

(defonce dropdown-element (atom nil))
(defonce rename-element (atom nil))
(defonce ideal-scroll-pos (atom c/settings-default-scroll-pos))
(defonce scroll-pos (atom nil))
(defonce saved-formbar-scroll-pos (atom 0))
(defonce down-settings-slider (atom nil))



(defn set-ideal-scroll-pos! [pos]
  (reset! ideal-scroll-pos pos))

(defn settings-circle [index]
  (let [center-circle (update (circle-within (app-rect))
                              :radius (partial * (base-zoom)))
        center-radius (:radius center-circle)]
    (add-points center-circle
                     (scale-point (global-attr :scroll-direction)
                                       (* (- index @scroll-pos)
                                          2
                                          center-radius
                                          c/outer-form-spacing)))))

(defn settings-circle-at [pos]
  (some #(when (in-circle? (settings-circle %)
                           pos)
           %)
        (range c/settings-pages)))

(defn on-stage? [index]
  (and @scroll-pos
       (< (Math/abs (- @ideal-scroll-pos index)) 0.01)))

(defn settings-button-circles []
  (vec
   (let [center-circle (settings-circle c/settings-project-selector-page)
         center-radius (:radius center-circle)
         bar-width (* 2 center-radius c/settings-project-dropdown-width)
         button-radius (* bar-width c/settings-project-button-radius)
         base-button-circle (-> center-circle
                                (update :y (partial +
                                                    (* center-radius
                                                       (+ c/settings-project-dropdown-y
                                                          c/settings-project-dropdown-height))
                                                    (* button-radius
                                                       (inc c/settings-project-buttons-y-spacing))))
                                (assoc :radius button-radius))]
     (for [index (range (count c/settings-project-buttons))]
       (update base-button-circle
               :x
               #(+ %
                   (* (- index
                         (* 0.5
                            (dec
                             (count c/settings-project-buttons))))
                      2 button-radius
                      (inc c/settings-project-buttons-x-spacing))))))))

(defn settings-quil-mode-circle []
  (let [center-circle (settings-circle c/settings-project-selector-page)
        center-radius (:radius center-circle)]
    (-> center-circle
        (update :x (partial + (* center-radius c/settings-project-quil-mode-circle-x)))
        (update :y (partial + (* center-radius c/settings-project-quil-mode-y)))
        (update :radius (partial * c/settings-project-quil-mode-circle-radius)))))

(defn settings-bar-scroll-circle []
  (let [center-circle (settings-circle c/settings-sliders-page)]
    (update (add-points center-circle
                             (scale-point (global-attr :scroll-direction)
                                               (* (:radius center-circle)
                                                  c/settings-bar-scroll-circle-pos)))
            :radius
            (partial * c/settings-bar-scroll-circle-radius))))

(defn settings-slider-at [pos]
  (some (fn [index]
          (let [slider-settings-circle (settings-circle c/settings-sliders-page)
                left (-> slider-settings-circle
                         (update :radius (partial * c/settings-slider-radius))
                         (update :x #(- % (* (:radius slider-settings-circle)
                                             c/settings-slider-width)))
                         (update :y (partial +
                                             (* (:radius slider-settings-circle)
                                                (+ c/settings-top-slider-y
                                                   (* c/settings-slider-spacing index))))))
                right (-> slider-settings-circle
                          (update :radius (partial * c/settings-slider-radius))
                          (update :x (partial +
                                              (* (:radius slider-settings-circle)
                                                 c/settings-slider-width)))
                          (update :y (partial +
                                              (* (:radius slider-settings-circle)
                                                 (+ c/settings-top-slider-y
                                                    (* c/settings-slider-spacing index))))))
                slider-rect [(update left :y #(- % (* (:radius slider-settings-circle)
                                                      c/settings-slider-radius)))
                             {:x (apply - (map :x [right left]))
                              :y (* 2 (:radius slider-settings-circle)
                                    c/settings-slider-radius)}]]
            (when (or (in-circle? right
                                  pos)
                      (in-circle? left
                                  pos)
                      (in-rect? slider-rect
                                pos))
              index)))
        (range (count c/settings-sliders))))

(defn color-scheme-index-at [pos]
  (if (on-stage? c/settings-color-scheme-page)
    (let [center-circle (settings-circle c/settings-color-scheme-page)
          center-radius (:radius center-circle)]
      (some (fn [index]
              (let [y (* center-radius
                         (+ c/settings-color-text-y
                            (* index c/settings-color-spacing)))
                    height (* center-radius
                              c/settings-color-height)
                    width (* center-radius
                             2
                             c/settings-color-width)]
                (when (or (in-rect? [(-> center-circle
                                         (update :y (partial + y (* -0.5 height)))
                                         (update :x (partial + (* -0.5 width))))
                                     {:x width
                                      :y height}]
                                    pos)
                          (in-circle? (-> center-circle
                                          (update :y (partial + y))
                                          (update :x (partial + (* -0.5 width)))
                                          (assoc :radius (* 0.5 height)))
                                      pos)
                          (in-circle? (-> center-circle
                                          (update :y (partial + y))
                                          (update :x (partial + (* 0.5 width)))
                                          (assoc :radius (* 0.5 height)))
                                      pos))
                  index)))
            (range (count c/color-schemes))))
    nil))



(defn refresh-dropdown-names []
  (let [dropdown @dropdown-element
        option-names (mapv :name (global-attr :projects))]
    (when dropdown
      (while (.-firstChild dropdown)
        (.removeChild dropdown
                      (.-lastChild dropdown)))
      (doseq [index (range (count option-names))]
        (let [name (nth option-names index)
              option (.createElement js/document "option")
              option-style (.-style option)]
          (set! (.-innerHTML option) name)
          (set! (.-backgroundColor option-style)
                (html-color (:highlight (color-scheme))))
          (set! (.-border option-style) "none")
          (set! (.-outline option-style) "none")
          (set! (.-box-shadow option-style) "none")
          (.appendChild dropdown option))))))

(defn hide-rename []
  (set! (.-display (.-style @rename-element)) "none"))

(defn hide-dropdown []
  (set! (.-display (.-style @dropdown-element)) "none"))

(defn activate-project-rename-input []
  (let [rename @rename-element]
    (set! (.-value rename) (project-attr :name))
    (set! (.-display (.-style rename)) "block")))

(defn saved-formbar-scroll-circles []
  (let [center-circle (settings-circle c/settings-saved-formbars-page)
        center-radius (:radius center-circle)
        scroll-bar-radius (* center-radius c/settings-saved-formbars-scroll-radius)
        scroll-x (+ (:x center-circle)
                    (* c/settings-saved-formbars-scroll-x center-radius))
        scroll-top-y (+ (:y center-circle)
                        (* center-radius c/settings-saved-formbars-box-y)
                        scroll-bar-radius)
        saved-formbar-box-height (+ (* 2 (* c/settings-saved-formbar-radius center-radius)
                                       c/settings-saved-formbars-box-height)
                                    (* center-radius
                                       c/saved-formbar-spacing
                                       c/settings-saved-formbar-radius
                                       (inc c/settings-saved-formbars-box-height)))
        scroll-bottom-y (+ (:y center-circle)
                           (* center-radius c/settings-saved-formbars-box-y)
                           scroll-bar-radius
                           (- saved-formbar-box-height (* 2 scroll-bar-radius)))]
    (vec
     (for [y [scroll-top-y scroll-bottom-y]]
       {:x scroll-x
        :y y
        :radius scroll-bar-radius}))))

(defn saved-formbar-scroll-rectangle []
  (let [[top-circle bottom-circle] (saved-formbar-scroll-circles)
        scroll-radius (:radius top-circle)]
    [(update top-circle
             :x #(- % scroll-radius))
     {:x (* 2 scroll-radius)
      :y (- (:y bottom-circle)
            (:y top-circle))}]))

(defn saved-formbar-index-at [pos]
  (let [saved-formbar-circle (settings-circle c/settings-saved-formbars-page)]
    (when (in-circle? saved-formbar-circle
                           pos)
      (let [center-radius (:radius saved-formbar-circle)
            formbar-radius (* center-radius c/settings-saved-formbar-radius)
            saved-formbar-box-width (+ (* 2 formbar-radius
                                          c/settings-saved-formbars-box-width)
                                       (* center-radius
                                          c/saved-formbar-spacing 2
                                          c/settings-saved-formbar-radius))
            saved-formbar-box-height (+ (* 2 formbar-radius
                                           c/settings-saved-formbars-box-height)
                                        (* center-radius
                                           c/saved-formbar-spacing
                                           c/settings-saved-formbar-radius
                                           (inc c/settings-saved-formbars-box-height)))
            formbar-zone-corner (-> saved-formbar-circle
                                    (select-keys [:x :y])
                                    (update :x (partial + (- (* c/settings-saved-formbars-box-x center-radius)
                                                             (* 0.5 saved-formbar-box-width))))
                                    (update :y (partial + (* center-radius c/settings-saved-formbars-box-y))))]
        (when (in-rect? [formbar-zone-corner
                              {:x saved-formbar-box-width
                               :y saved-formbar-box-height}]
                             pos)
          (int
           (u/clamp 0 (dec c/settings-saved-formbars-box-height)
                    (u/map-range 0 saved-formbar-box-height
                                 0 (- c/settings-saved-formbars-box-height
                                      (* center-radius
                                         c/saved-formbar-spacing
                                         c/settings-saved-formbar-radius))
                                 (- (:y pos)
                                    (:y formbar-zone-corner)
                                    (* center-radius
                                       c/saved-formbar-spacing
                                       0.5
                                       c/settings-saved-formbar-radius))))))))))

(defn saved-formbar-insertion-index-at [pos]
  (let [saved-formbar-circle (settings-circle c/settings-saved-formbars-page)]
    (when (in-circle? saved-formbar-circle
                           pos)
      (let [center-radius (:radius saved-formbar-circle)
            formbar-radius (* center-radius c/settings-saved-formbar-radius)
            saved-formbar-box-width (+ (* 2 formbar-radius
                                          c/settings-saved-formbars-box-width)
                                       (* center-radius
                                          c/saved-formbar-spacing 2
                                          c/settings-saved-formbar-radius))
            saved-formbar-box-height (+ (* 2 formbar-radius
                                           c/settings-saved-formbars-box-height)
                                        (* center-radius
                                           c/saved-formbar-spacing
                                           c/settings-saved-formbar-radius
                                           (inc c/settings-saved-formbars-box-height)))
            formbar-zone-corner (-> saved-formbar-circle
                                    (select-keys [:x :y])
                                    (update :x (partial + (- (* c/settings-saved-formbars-box-x center-radius)
                                                             (* 0.5 saved-formbar-box-width))))
                                    (update :y (partial + (* center-radius c/settings-saved-formbars-box-y))))]
        (when (in-rect? [formbar-zone-corner
                              {:x saved-formbar-box-width
                               :y saved-formbar-box-height}]
                             pos)
          (int
           (u/clamp 0 c/settings-saved-formbars-box-height
                    (+ 0.5
                       (u/map-range 0 saved-formbar-box-height
                                    0 (- c/settings-saved-formbars-box-height
                                         (* center-radius
                                            c/saved-formbar-spacing
                                            c/settings-saved-formbar-radius))
                                    (- (:y pos)
                                       (:y formbar-zone-corner)
                                       (* center-radius
                                          c/saved-formbar-spacing
                                          0.5
                                          c/settings-saved-formbar-radius)))))))))))

(defn formbar-tool-circles []
  (let [center-circle (settings-circle c/settings-formbar-tools-page)
        center-radius (:radius center-circle)]
    (mapv (fn [y]
            (let [row (c/settings-formbar-tool-types y)
                  row-count (count row)]
              (mapv (fn [x]
                      (-> (select-keys center-circle [:x :y :radius])
                          (update :x (partial + (* center-radius
                                                   c/settings-formbar-tool-radius
                                                   2
                                                   c/settings-formbar-tool-x-spacing
                                                   (- x
                                                      (* 0.5 (dec row-count))))))
                          (update :y (partial +
                                              (* center-radius
                                                 c/settings-formbar-tool-y)
                                              (* y
                                                 center-radius
                                                 c/settings-formbar-tool-radius
                                                 2
                                                 c/settings-formbar-tool-y-spacing)))
                          (assoc :radius (* center-radius c/settings-formbar-tool-radius))
                          (assoc :type (row x))))
                    (range row-count))))
          (range (count c/settings-formbar-tool-types)))))

(defn tool-circle-at [pos]
  (some #(when (in-circle? % pos)
           (:type %))
        (apply concat
               (formbar-tool-circles))))

(defn init []
  (u/log "Settings Page Initializing...")

  (let [dropdown (.createElement js/document "select")
        style (.-style dropdown)]
    (set! (.-onchange dropdown)
          #(do (load-project (.-selectedIndex dropdown))
               (refresh-dropdown-names)))
    (.appendChild (.-body js/document) dropdown)
    (set! (.-textAlign style) "center")
    (set! (.-position style) "absolute")
    (set! (.-fontFamily style) c/font-name)
    (set! (.-background style) "transparent")
    (set! (.-border style) "none")
    (set! (.-outline style) "none")
    (reset! dropdown-element dropdown))
  (let [project-rename-input (.createElement js/document "input")
        style (.-style project-rename-input)]
    (set! (.-onchange project-rename-input)
          #(do (set-project-attr! :name (.-value project-rename-input))
               (refresh-dropdown-names)
               (hide-rename)
               (hide-dropdown)))
    (.appendChild (.-body js/document) project-rename-input)
    (set! (.-textAlign style) "center")
    (set! (.-position style) "absolute")
    (set! (.-fontFamily style) c/font-name)
    (set! (.-background style) "transparent")
    (set! (.-border style) "none")
    (set! (.-outline style) "none")
    (reset! rename-element project-rename-input)
    (hide-rename))
  (register-page!
   :settings
   {:enter
    (fn []
      (refresh-dropdown-names)
      (reset! scroll-pos @ideal-scroll-pos))
    
    :exit
    (fn []
      (hide-rename)
      (hide-dropdown)
      (reset! scroll-pos nil))

    :resize-html
    (fn []
      (let [current-size (app-size)
            dropdown @dropdown-element
            rename @rename-element]
        (when dropdown
          (let [{:keys [x y radius]} (settings-circle c/settings-project-selector-page)
                dropdown-width (* current-size
                                  radius
                                  c/settings-project-dropdown-width)
                new-text-size (* current-size
                                 c/text-scale-factor
                                 c/html-text-size-factor
                                 c/settings-project-name-size
                                 radius)
                style (.-style dropdown)]
            (set! (.-display style)
                  (if (and (= (.-display (.-style rename))
                              "none")
                           (on-stage? c/settings-project-selector-page))
                    "block"
                    "none"))
            (set! (.-left style)
                  (str (- (screen-x x)
                          dropdown-width)
                       "px"))
            (set! (.-top style)
                  (str (screen-y
                        (+ y (* c/settings-project-dropdown-y radius)))
                       "px"))
            (set! (.-width style)
                  (str (* 2 dropdown-width)
                       "px"))
            (set! (.-height style)
                  (str (* current-size
                          radius
                          c/settings-project-dropdown-height)
                       "px"))
            (set! (.-fontSize style)
                  (str new-text-size
                       "px"))))
        (when rename
          (let [{:keys [x y radius]} (settings-circle c/settings-project-selector-page)
                dropdown-width (* current-size
                                  radius
                                  c/settings-project-dropdown-width)
                new-text-size (* current-size
                                 c/text-scale-factor
                                 c/html-text-size-factor
                                 c/settings-project-name-size
                                 radius)
                style (.-style rename)]
            (when (and (not (on-stage? c/settings-project-selector-page))
                       (= (.-display (.-style rename))
                          "block"))
              (refresh-dropdown-names)
              (hide-rename))
            (set! (.-left style)
                  (str (- (screen-x x)
                          dropdown-width)
                       "px"))
            (set! (.-top style)
                  (str (screen-y
                        (+ y (* c/settings-project-dropdown-y radius)))
                       "px"))
            (set! (.-width style)
                  (str (* 2 dropdown-width)
                       "px"))
            (set! (.-height style)
                  (str (* current-size
                          radius
                          c/settings-project-dropdown-height)
                       "px"))
            (set! (.-fontSize style)
                  (str new-text-size
                       "px"))))))

    :refresh-html-colors
    (fn []
      (refresh-dropdown-names)
      (doseq [html-object [@dropdown-element @rename-element]]
        (set! (.-color (.-style html-object))
              (html-color (:text (color-scheme))))))

    :mouse-zone
    (fn [mouse]
      (let [[app-pos app-size] (app-rect)
            button-circles (settings-button-circles)]
        (or (when (on-stage? c/settings-project-selector-page)
              (some (fn [index]
                      (when (in-circle? (nth button-circles index)
                                        mouse)
                        (nth c/settings-project-buttons index)))
                    (range (count button-circles))))
            (cond
              (and (:down? mouse)
                   (#{:formbar :saved-formbar}
                    (:down-zone mouse))
                   (in-discard-corner? mouse))
              :discard

              (<= (point-magnitude
                   (subtract-points app-pos
                                    mouse))
                  c/upper-corner-zone-radius)
              :back-icon

              (formbar-path-at mouse)
              :formbar

              (new-formbar-circle-path-at mouse)
              :new-formbar

              (and (on-stage? c/settings-sliders-page)
                   (in-circle? (settings-bar-scroll-circle) mouse))
              :scroll-circle

              (and (on-stage? c/settings-sliders-page)
                   (settings-slider-at mouse))
              :settings-slider

              (and (on-stage? c/settings-saved-formbars-page)
                   (or (reduce #(or %1
                                    (in-circle? %2 mouse))
                               false
                               (saved-formbar-scroll-circles))
                       (in-rect? (saved-formbar-scroll-rectangle) mouse)))
              :saved-formbar-scroll

              (color-scheme-index-at mouse)
              :color-scheme

              (saved-formbar-index-at mouse)
              :saved-formbar

              (and (on-stage? c/settings-formbar-tools-page)
                   (tool-circle-at mouse))
              :tool-circle

              (settings-circle-at mouse)
              :settings-circle

              :else :empty))))

    :render
    (fn [mouse mouse-zone]
      (render-formbars mouse)
      (doseq [i (range c/settings-pages)]
        (draw-circle (settings-circle i)
                     (:foreground (color-scheme))
                     :background))

     ;; Sliders
      (let [center-circle (settings-circle c/settings-sliders-page)
            center-radius (:radius center-circle)]
        (doseq [slider-index (range (count c/settings-sliders))]
          (let [[slider-name slider-key] (nth c/settings-sliders slider-index)
                y (* center-radius
                     (+ c/settings-top-slider-y
                        (* slider-index c/settings-slider-spacing)))
                left (add-points center-circle
                                 {:x (* -1
                                        center-radius
                                        c/settings-slider-width)
                                  :y y})
                right (add-points center-circle
                                  {:x (* center-radius
                                         c/settings-slider-width)
                                   :y y})]
            (draw-line left right
                       (* center-radius
                          c/settings-slider-radius
                          2)
                       (:background (color-scheme))
                       :background)
            (doseq [p [right left]]
              (draw-circle (assoc p
                                  :radius (* center-radius
                                             c/settings-slider-radius))
                           (:background (color-scheme))
                           :background))
            (draw-circle (assoc (tween-points left right (global-attr slider-key))
                                :radius (* center-radius
                                           c/settings-slider-radius
                                           c/settings-slider-inner-radius-factor))
                         (:foreground (color-scheme))
                         :background)
            (draw-text slider-name
                       (add-points center-circle
                                   {:y (+ y (* center-radius c/settings-slider-text-y))})
                       (* center-radius
                          c/settings-slider-text-size
                          (count slider-name))
                       (:text (color-scheme))
                       :background))))

     ;; Render formbar tools page
      (let [center-circle (settings-circle c/settings-formbar-tools-page)
            center-radius (:radius center-circle)]
        (draw-text "Tools"
                   (-> center-circle
                       (update :y (partial + (* center-radius c/settings-formbar-tool-text-y))))
                   (* center-radius c/settings-formbar-tool-text-size)
                   (:text (color-scheme))
                   :background)
        (doseq [row (formbar-tool-circles)]
          (doseq [circle row]
            (draw-circle circle
                         (:background (color-scheme))
                         :background)
            (when (on-stage? c/settings-formbar-tools-page)
              (render-tool (:type circle)
                           circle)))))

     ;; Render saved formbar page
      (let [center-circle (settings-circle c/settings-saved-formbars-page)
            center-radius (:radius center-circle)]
        (draw-text "Saved Formbars"
                   (-> center-circle
                       (update :y (partial + (* center-radius c/settings-saved-formbars-text-y))))
                   (* center-radius c/settings-saved-formbars-text-size)
                   (:text (color-scheme))
                   :background)
        (let [formbar-radius (* center-radius c/settings-saved-formbar-radius)
              saved-formbar-box-width (+ (* 2 formbar-radius
                                            c/settings-saved-formbars-box-width)
                                         (* center-radius
                                            c/saved-formbar-spacing 2
                                            c/settings-saved-formbar-radius))
              saved-formbar-box-height (+ (* 2 formbar-radius
                                             c/settings-saved-formbars-box-height)
                                          (* center-radius
                                             c/saved-formbar-spacing
                                             c/settings-saved-formbar-radius
                                             (inc c/settings-saved-formbars-box-height)))
              formbar-zone-corner (-> center-circle
                                      (select-keys [:x :y])
                                      (update :x (partial + (- (* c/settings-saved-formbars-box-x center-radius)
                                                               (* 0.5 saved-formbar-box-width))))
                                      (update :y (partial + (* center-radius c/settings-saved-formbars-box-y))))]
         ; Formbar area background
          (let [corner-radius (* (inc c/saved-formbar-spacing) center-radius c/saved-formbar-zone-corner-radius)]
            (doseq [y [corner-radius
                       (- saved-formbar-box-height corner-radius)]]
              (draw-circle (-> formbar-zone-corner
                               (assoc :radius corner-radius)
                               (update :x (partial + corner-radius))
                               (update :y (partial + y)))
                           (:background (color-scheme))
                           :background)
              (draw-circle (-> formbar-zone-corner
                               (assoc :radius corner-radius)
                               (update :x (partial + (- saved-formbar-box-width corner-radius)))
                               (update :y (partial + y)))
                           (:background (color-scheme))
                           :background))
            (draw-rect [(-> formbar-zone-corner
                            (update :x (partial + corner-radius)))
                        {:x (- saved-formbar-box-width
                               (* 2 corner-radius))
                         :y saved-formbar-box-height}]
                       (:background (color-scheme))
                       :background)
            (draw-rect [(-> formbar-zone-corner
                            (update :y (partial + corner-radius)))
                        {:x saved-formbar-box-width
                         :y (- saved-formbar-box-height
                               (* 2 corner-radius))}]
                       (:background (color-scheme))
                       :background))

         ; Saved formbars
          (let [saved-formbar-contents (saved-formbar-contents)
                formbar-index-offset @saved-formbar-scroll-pos
                hovered-formbar-index (saved-formbar-index-at mouse)]
            (doseq [formbar-index (map (partial + formbar-index-offset)
                                       (range (min (- (count saved-formbar-contents) formbar-index-offset)
                                                   c/settings-saved-formbars-box-height)))]
              (let [adjusted-formbar-index (- formbar-index formbar-index-offset)
                    formbar-forms (nth saved-formbar-contents formbar-index)
                    formbar-oversized (> (count formbar-forms)
                                         c/settings-saved-formbars-box-width)
                    draw-bar (fn [scale color layer]
                               (doseq [i (if formbar-oversized
                                           [0]
                                           [0 (dec (count formbar-forms))])]
                                 (draw-circle (-> formbar-zone-corner
                                                  (assoc :radius (* scale formbar-radius))
                                                  (update :x (partial +
                                                                      (* formbar-radius
                                                                         (inc (+ c/saved-formbar-spacing
                                                                                 (* 2 i))))))
                                                  (update :y (partial +
                                                                      (* formbar-radius
                                                                         (inc (+ (* c/saved-formbar-spacing
                                                                                    (inc adjusted-formbar-index))
                                                                                 (* 2 adjusted-formbar-index)))))))
                                              color
                                              layer))
                               (draw-rect [(-> formbar-zone-corner
                                               (update :x (partial +
                                                                   (* formbar-radius
                                                                      (inc c/saved-formbar-spacing))))
                                               (update :y #(- (+ % (* formbar-radius
                                                                      (inc (+ (* c/saved-formbar-spacing
                                                                                 (inc adjusted-formbar-index))
                                                                              (* 2 adjusted-formbar-index)))))
                                                              (* scale formbar-radius))))
                                           {:x (if formbar-oversized
                                                 (- saved-formbar-box-width
                                                    (* formbar-radius
                                                       (inc c/saved-formbar-spacing)))
                                                 (* formbar-radius 2 (dec (count formbar-forms))))
                                            :y (* scale formbar-radius 2)}]
                                          color
                                          layer))]
                (draw-bar 1
                          (:foreground (color-scheme))
                          :program)
                (draw-bar (- 1 c/formbar-outline-thickness)
                          (:background (color-scheme))
                          :program)
                (doseq [form-index (range (count formbar-forms))]
                  (when (< form-index c/settings-saved-formbars-box-width)
                    (render-sublayouts
                     (form-layout (nth formbar-forms form-index)
                                  (-> formbar-zone-corner
                                      (assoc :radius (* c/formbar-form-size (- 1 c/formbar-outline-thickness) formbar-radius))
                                      (update :x (partial +
                                                          (* formbar-radius
                                                             (inc (+ c/saved-formbar-spacing
                                                                     (* 2 form-index))))))
                                      (update :y (partial +
                                                          (* formbar-radius
                                                             (inc (+ (* c/saved-formbar-spacing
                                                                        (inc adjusted-formbar-index))
                                                                     (* 2 adjusted-formbar-index))))))))
                     :program)))
                (when (and (not (:down? mouse))
                           (= adjusted-formbar-index hovered-formbar-index))
                  (draw-bar 1
                            (:highlight (color-scheme))
                            :program-overlay))))
            (when (and (:down? mouse)
                       (or (= :formbar (:down-zone mouse))
                           (and (= :saved-formbar (:down-zone mouse))
                                (< (+ (saved-formbar-index-at (:down-pos mouse)) @saved-formbar-scroll-pos)
                                   (count (saved-formbar-contents))))))
              (let [insertion-index (min (saved-formbar-insertion-index-at mouse)
                                         (count (saved-formbar-contents)))]
                (when insertion-index
                  (let [y-offset (+ (* insertion-index
                                       formbar-radius
                                       2)
                                    (* (+ 0.5 insertion-index)
                                       formbar-radius
                                       c/saved-formbar-spacing))
                        x-offset (* saved-formbar-box-width 0.5
                                    (- 1 c/settings-saved-formbar-insertion-bar-width))]
                    (draw-line (-> formbar-zone-corner
                                   (update :x (partial + x-offset))
                                   (update :y (partial + y-offset)))
                               (-> formbar-zone-corner
                                   (update :x (partial + (- saved-formbar-box-width x-offset)))
                                   (update :y (partial + y-offset)))
                               c/settings-saved-formbar-insertion-bar-thickness
                               (:highlight (color-scheme))
                               :program-overlay))))))

         ; Saved Formbar Slider
          (draw-rect (saved-formbar-scroll-rectangle)
                     (:background (color-scheme))
                     :background)
          (doseq [scroll-circle (saved-formbar-scroll-circles)]
            (draw-circle scroll-circle
                         (:background (color-scheme))
                         :background))
          (when (> (count (saved-formbar-contents))
                   c/settings-saved-formbars-box-height)
            (draw-circle (update (apply tween-points
                                        (conj (saved-formbar-scroll-circles)
                                              (/ @saved-formbar-scroll-pos
                                                 (max 0
                                                      (- (count (saved-formbar-contents))
                                                         c/settings-saved-formbars-box-height)))))
                                 :radius (partial * c/settings-slider-inner-radius-factor))
                         (:foreground (color-scheme))
                         :background))))

     ;; Render project dropdown
      (let [center-circle (settings-circle c/settings-project-selector-page)
            center-radius (:radius center-circle)
            bar-width (* 2 center-radius c/settings-project-dropdown-width)
            bar-height (* center-radius c/settings-project-dropdown-height)]
        (draw-rect [(-> center-circle
                        (update :x #(+ (- % (* center-radius c/settings-project-dropdown-width))
                                       (* bar-height
                                          c/settings-project-dropdown-x-shrink-factor)))
                        (update :y (partial +
                                            (* center-radius c/settings-project-dropdown-y))))
                    {:x (- bar-width
                           (* bar-height
                              2
                              c/settings-project-dropdown-x-shrink-factor))
                     :y bar-height}]
                   (:highlight (color-scheme))
                   :background)
        (draw-circle (-> center-circle
                         (update :x #(+ (- % (* center-radius c/settings-project-dropdown-width))
                                        (* bar-height
                                           c/settings-project-dropdown-x-shrink-factor)))
                         (update :y (partial +
                                             (* center-radius c/settings-project-dropdown-y)
                                             (* 0.5 bar-height)))
                         (assoc :radius (* 0.5 bar-height)))
                     (:highlight (color-scheme))
                     :background)
        (draw-circle (-> center-circle
                         (update :x #(+ (- %
                                           (* center-radius c/settings-project-dropdown-width)
                                           (* bar-height
                                              c/settings-project-dropdown-x-shrink-factor))
                                        bar-width))
                         (update :y (partial +
                                             (* center-radius c/settings-project-dropdown-y)
                                             (* 0.5 bar-height)))
                         (assoc :radius (* 0.5 bar-height)))
                     (:highlight (color-scheme))
                     :background)
        (draw-text "Active Project"
                   (-> center-circle
                       (update :y (partial + (* center-radius (+ c/settings-project-dropdown-y
                                                                 c/settings-project-text-y)))))
                   (* center-radius c/settings-project-text-size)
                   (:text (color-scheme))
                   :background)
        (when (not (on-stage? c/settings-project-selector-page))
          (draw-text (project-attr :name)
                     (-> center-circle
                         (update :y (partial + (* center-radius
                                                  (+ c/settings-project-dropdown-y
                                                     (* 0.5
                                                        c/settings-project-dropdown-height))))))
                     (* center-radius
                        c/settings-project-name-size
                        (count (project-attr :name)))
                     (:text (color-scheme))
                     :background))

        (let [button-circles (settings-button-circles)]
          (doseq [index (range (count c/settings-project-buttons))]
            (let [type (nth c/settings-project-buttons index)
                  button-circle (nth button-circles index)
                  button-radius (:radius button-circle)]
              (draw-circle button-circle
                           (:background (color-scheme))
                           :background)
              (draw-circle (update button-circle
                                   :radius
                                   (partial * c/settings-project-button-inner-radius-factor))
                           (if (= mouse-zone type)
                             (:highlight (color-scheme))
                             (:foreground (color-scheme)))
                           :background)
              (case type
                :rename-project
                (let [eraser-offset (update (scale-point unit
                                                         (* button-radius
                                                            (Math/sqrt 0.5)
                                                            c/settings-project-button-inner-radius-factor
                                                            c/rename-icon-size))
                                            :y -)
                      width (* button-radius
                               c/settings-project-button-inner-radius-factor
                               c/rename-icon-width)
                      line-width (* button-radius
                                    c/settings-button-line-width)
                      eraser (add-points button-circle
                                         eraser-offset)
                      eraser-top (add-points eraser
                                             (scale-point unit
                                                          (* (Math/sqrt 0.5)
                                                             -0.5
                                                             width)))
                      eraser-bottom (add-points eraser
                                                (scale-point unit
                                                             (* (Math/sqrt 0.5)
                                                                0.5
                                                                width)))
                      eraser-edge-top (add-points eraser-top
                                                  (scale-point eraser-offset
                                                               (- c/rename-icon-eraser-size)))
                      eraser-edge-bottom (add-points eraser-bottom
                                                     (scale-point eraser-offset
                                                                  (- c/rename-icon-eraser-size)))
                      tip-top (add-points eraser-top
                                          (scale-point eraser-offset
                                                       (- c/rename-icon-tip-size
                                                          (inc c/rename-icon-tip-factor))))
                      tip-bottom (add-points eraser-bottom
                                             (scale-point eraser-offset
                                                          (- c/rename-icon-tip-size
                                                             (inc c/rename-icon-tip-factor))))
                      tip (add-points button-circle
                                      (scale-point eraser-offset
                                                   (- c/rename-icon-tip-factor)))]
                  (draw-polyline [tip
                                  tip-bottom
                                  eraser-bottom
                                  eraser-top
                                  tip-top
                                  tip]
                                 line-width
                                 (:background (color-scheme))
                                 :background)
                  (draw-line eraser-edge-top
                             eraser-edge-bottom
                             line-width
                             (:background (color-scheme))
                             :background)
                  (draw-line tip-top
                             tip-bottom
                             line-width
                             (:background (color-scheme))
                             :background))

                :new-project
                (let [size (* button-radius
                              c/settings-project-button-inner-radius-factor
                              c/new-icon-size)
                      width (* button-radius
                               c/settings-project-button-inner-radius-factor
                               c/new-icon-width)]
                  (doseq [dim [:x :y]]
                    (draw-line (update button-circle
                                       dim
                                       (partial + (- size)))
                               (update button-circle
                                       dim
                                       (partial + size))
                               width
                               (:background (color-scheme))
                               :background)))

                :duplicate-project
                (let [width (* button-radius
                               c/duplicate-icon-width)
                      height (* button-radius
                                c/duplicate-icon-height)
                      line-width (* button-radius
                                    c/settings-button-line-width)
                      corner {:x width
                              :y height}
                      base-offset (scale-point unit
                                               (* button-radius
                                                  c/duplicate-icon-offset))]
                  (doseq [offset [(scale-point base-offset -1)
                                  base-offset]]
                    (let [base (add-points button-circle offset)]
                      (draw-rect [(add-points base
                                              (scale-point corner -1))
                                  (scale-point corner 2)]
                                 (if (= mouse-zone :duplicate-project)
                                   (:highlight (color-scheme))
                                   (:foreground (color-scheme)))
                                 :background)
                      (draw-polyline (mapv (fn [dims]
                                             (add-points base
                                                         (reduce #(update %1 %2 -)
                                                                 corner
                                                                 dims)))
                                           [[]
                                            [:x]
                                            [:x :y]
                                            [:y]
                                            []])
                                     line-width
                                     (:background (color-scheme))
                                     :background))))

                :delete-project
                (let [offset (scale-point unit
                                          (* button-radius
                                             (Math/sqrt 0.5)
                                             c/settings-project-button-inner-radius-factor
                                             c/new-icon-size))
                      width (* button-radius
                               c/settings-project-button-inner-radius-factor
                               c/new-icon-width)]
                  (doseq [offset-modifier [identity #(update % :y -)]]
                    (let [modified-offset (offset-modifier offset)]
                      (draw-line (add-points button-circle
                                             (scale-point modified-offset
                                                          -1))
                                 (add-points button-circle
                                             modified-offset)
                                 width
                                 (:background (color-scheme))
                                 :background))))))))
        (draw-text "Quil Mode"
                   (-> center-circle
                       (update :x (partial + (* center-radius c/settings-project-quil-mode-text-x)))
                       (update :y (partial + (* center-radius c/settings-project-quil-mode-y))))
                   (* center-radius c/settings-project-quil-mode-text-size)
                   (:text (color-scheme))
                   :background)
        
        (draw-circle (settings-quil-mode-circle)
                     (:background (color-scheme))
                     :background)
        (draw-circle (update (settings-quil-mode-circle)
                             :radius (partial * (- 1 c/settings-project-quil-mode-circle-outline-factor)))
                     (:highlight (color-scheme))
                     :background))

     ;; Render scroll circle
      (let [scroll-circle (settings-bar-scroll-circle)
            scroll-direction (global-attr :scroll-direction)
            triangle-base (add-points scroll-circle
                                      (scale-point scroll-direction
                                                   (* (:radius scroll-circle)
                                                      c/settings-bar-scroll-triangle-pos)))
            color (if (= mouse-zone :scroll-circle)
                    (:highlight (color-scheme))
                    (:foreground (color-scheme)))]
        (draw-circle scroll-circle
                     color
                     :background)
        (draw-circle (update scroll-circle
                             :radius
                             (partial * c/settings-bar-scroll-circle-inner-radius))
                     (:background (color-scheme))
                     :background)
        (draw-polygon (mapv #(add-points triangle-base
                                         (scale-point %
                                                      (:radius scroll-circle)))
                            [(scale-point scroll-direction
                                          c/settings-bar-scroll-triangle-height)
                             (scale-point (assoc scroll-direction
                                                 :x (:y scroll-direction)
                                                 :y (- (:x scroll-direction)))
                                          c/settings-bar-scroll-triangle-width)
                             (scale-point (assoc scroll-direction
                                                 :x (- (:y scroll-direction))
                                                 :y (:x scroll-direction))
                                          c/settings-bar-scroll-triangle-width)])
                      color
                      :background))

     ;; Render color scheme page
      (let [center-circle (settings-circle c/settings-color-scheme-page)
            center-radius (:radius center-circle)]
        (draw-text "Color Scheme"
                   (-> center-circle
                       (update :y
                               (partial +
                                        (* center-radius
                                           c/settings-color-header-text-y))))
                   (* center-radius c/settings-color-header-text-size)
                   (:text (color-scheme))
                   :background)

        (doseq [i (range (count c/color-schemes))]
          (let [color-scheme (nth c/color-schemes i)
                name (:name color-scheme)
                y (* center-radius
                     (+ c/settings-color-text-y
                        (* i c/settings-color-spacing)))
                display-colors (mapv #(% color-scheme)
                                     [:background :foreground :highlight])]
            (doseq [[color layer]
                    (if (= i (color-scheme-index-at mouse))
                      [[(:highlight color-scheme) 0]]
                      (mapv vector
                            display-colors
                            (range (count display-colors))))]
              (let [width (* 2
                             (- 1 (* layer c/settings-color-width-factor))
                             c/settings-color-width
                             center-radius)
                    height (* c/settings-color-height
                              (- 1 (* layer c/settings-color-height-factor))
                              center-radius)]
                (draw-rect [(-> center-circle
                                (update :y #(- (+ % y)
                                               (* 0.5
                                                  height)))
                                (update :x #(- % (* 0.5 width))))
                            {:x width
                             :y height}]
                           color
                           :background)
                (doseq [side [1 -1]]
                  (draw-circle (-> center-circle
                                   (update :y (partial + y))
                                   (update :x (partial + (* 0.5
                                                            side
                                                            width)))
                                   (assoc :radius (* height 0.5)))
                               color
                               :background))))
            (draw-text name
                       (-> center-circle
                           (update :y
                                   (partial +
                                            y)))
                       (* center-radius
                          c/settings-color-text-size
                          (count name))
                       (:text color-scheme)
                       :background))))

     ;; Render dragged tools
      (when (and (:dragging? mouse)
                 (= (:down-zone mouse) :tool-circle))
        (let [formbar-insertion-path (formbar-insertion-path-at mouse)]
          (render-tool (tool-circle-at (:down-pos mouse))
                       (if formbar-insertion-path
                         (formbar-insertion-circle formbar-insertion-path)
                         (assoc mouse
                                :radius c/settings-formbar-drag-tool-radius))
                       true)))

     ;; Render new formbar circles
      (doseq [[new-formbar-circle] (new-formbar-circles)]
        (draw-circle new-formbar-circle
                     (:highlight (color-scheme))
                     :settings-overlay)
        (let [line-size (* (formbar-radius)
                           c/new-formbar-circle-radius
                           c/new-icon-size)]
          (doseq [dim [:x :y]]
            (draw-line (update new-formbar-circle dim (partial + line-size))
                       (update new-formbar-circle dim #(- % line-size))
                       (* (formbar-radius)
                          c/new-formbar-circle-radius
                          c/new-icon-width)
                       (:background (color-scheme))
                       :settings-overlay))))

     ;; Render formbar overlays
      (let [formbar-path (formbar-path-at mouse)]
        (when formbar-path
          (let [current-formbar-arrangement (formbar-arrangement)
                hovered-formbar (get-in current-formbar-arrangement formbar-path)
                {:keys [width height]} hovered-formbar
                center (add-points hovered-formbar
                                   (scale-point {:x width
                                                 :y height}
                                                0.5))]
            (draw-circle (assoc hovered-formbar
                                :radius (formbar-radius))
                         (:highlight (color-scheme))
                         :settings-overlay)
            (when (pos? width)
              (draw-circle (-> hovered-formbar
                               (update :x (partial + width))
                               (assoc :radius (formbar-radius)))
                           (:highlight (color-scheme))
                           :settings-overlay)
              (draw-rect [(update hovered-formbar
                                  :y #(- % (formbar-radius)))
                          {:x width
                           :y (* 2 (formbar-radius))}]
                         (:highlight (color-scheme))
                         :settings-overlay))
            (when (pos? height)
              (draw-circle (-> hovered-formbar
                               (update :y (partial + height))
                               (assoc :radius (formbar-radius)))
                           (:highlight (color-scheme))
                           :settings-overlay)
              (draw-rect [(update hovered-formbar
                                  :x #(- % (formbar-radius)))
                          {:x (* 2 (formbar-radius))
                           :y height}]
                         (:highlight (color-scheme))
                         :settings-overlay)))))
      (let [formbar-insertion-path (formbar-insertion-path-at mouse)]
        (when (:down? mouse)
          (let [{:keys [down-zone]} mouse]
            (when (or (= :formbar down-zone)
                      (and (= :saved-formbar down-zone)
                           (< (+ (saved-formbar-index-at (:down-pos mouse)) @saved-formbar-scroll-pos)
                              (count (saved-formbar-contents)))))
              (when (not= :bindings
                          (:type
                           (get-in (project-attr :formbars)
                                   (formbar-path-at (:down-pos mouse)))))
                (render-discard-zone (= mouse-zone :discard) true))
              (when (and (not (= mouse-zone :discard))
                         formbar-insertion-path)
                (let [formbar-insertion-circle (formbar-insertion-circle formbar-insertion-path)]
                  (draw-circle formbar-insertion-circle
                               (:foreground (color-scheme))
                               :settings-overlay)
                  (draw-circle (update formbar-insertion-circle
                                       :radius (partial * 0.9))
                               (:background (color-scheme))
                               :settings-overlay)))))))

      (render-top-left-button-background (= mouse-zone :back-icon))
      (render-top-left-back-button))

    :update
    (fn [delta mouse]
      (when (:down? mouse)
        (when (= (:down-zone mouse) :settings-slider)
          (let [settings-circle (settings-circle c/settings-sliders-page)
                x-off (apply - (map :x [mouse settings-circle]))
                adjusted-x-off (/ x-off (* (:radius settings-circle) c/settings-slider-width))]
            (when @down-settings-slider
              (set-global-attr! (second
                                 (nth c/settings-sliders
                                      @down-settings-slider))
                                (min 1
                                     (max 0
                                          (* 0.5
                                             (inc adjusted-x-off))))))))
        (when (= (:down-zone mouse) :scroll-circle)
          (set-global-attr! :scroll-direction
                            (angle-point
                             (let [raw-angle (mod (point-angle
                                                   (subtract-points mouse
                                                                    (settings-circle c/settings-project-selector-page)))
                                                  TAU)
                                   snap-angle (some (fn [angle]
                                                      (when (< (min (Math/abs (- angle raw-angle))
                                                                    (- TAU
                                                                       (Math/abs (- angle raw-angle))))
                                                               c/scroll-angle-snap-distance)
                                                        angle))
                                                    (mapv (partial * TAU)
                                                          (u/prop-range c/scroll-angle-snap-positions true)))]
                               (or snap-angle
                                   raw-angle))))))
      (swap! ideal-scroll-pos
             #(max 0
                   (min (dec c/settings-pages)
                        %)))
      (when @scroll-pos
        (swap! scroll-pos
               #(u/tween @ideal-scroll-pos
                         %
                         (Math/pow (:move (camera-speed 0))
                                   delta))))
      (when (and (:down? mouse)
                 (= (:down-zone mouse)
                    :saved-formbar-scroll))
        (let [[scroll-top-y scroll-bottom-y] (mapv :y (saved-formbar-scroll-circles))
              scroll-pos (u/clamp (u/map-range scroll-top-y scroll-bottom-y 0 1 (:y mouse)))
              saved-formbar-overflow-count (max 0
                                                (- (count (saved-formbar-contents))
                                                   c/settings-saved-formbars-box-height))]
          (reset! saved-formbar-scroll-pos
                  (min (int (* (inc saved-formbar-overflow-count) scroll-pos))
                       saved-formbar-overflow-count)))))

    :scroll
    (fn [diff]
      (swap! ideal-scroll-pos (partial + diff)))

    :click-down
    (fn [mouse mouse-zone]
      (reset! down-settings-slider
              (settings-slider-at mouse))
      (when (= mouse-zone :settings-circle)
        (set-ideal-scroll-pos! (settings-circle-at mouse))))

    :click-up
    (fn [mouse mouse-zone]
      (cond
        (= mouse-zone :discard)
        (case (:down-zone mouse)
          :formbar
          (let [dragged-formbar-path (formbar-path-at
                                      (:down-pos mouse))]
            (when-not (= :bindings
                         (:type
                          (get-in (project-attr :formbars)
                                  dragged-formbar-path)))
              (delete-project-formbar-at dragged-formbar-path)))

          :saved-formbar
          (let [adjusted-formbar-index (+ (saved-formbar-index-at (:down-pos mouse)) @saved-formbar-scroll-pos)]
            (when (< adjusted-formbar-index
                     (count (saved-formbar-contents)))
              (delete-saved-formbar! adjusted-formbar-index)
              (swap! saved-formbar-scroll-pos
                     (fn [pos]
                       (u/clamp 0 (- (count (saved-formbar-contents))
                                     c/settings-saved-formbars-box-height)
                                pos)))))
          nil))
      (if (:dragging? mouse)
        (cond
          (= (:down-zone mouse) :formbar)
          (let [formbar-placement-path (formbar-insertion-path-at mouse)
                saved-formbar-insertion-index (saved-formbar-insertion-index-at mouse)]
            (when saved-formbar-insertion-index
              (let [dragged-formbar (get-in (project-attr :formbars)
                                            (formbar-path-at (:down-pos mouse)))]
                (when (not (:type dragged-formbar))
                  (add-saved-formbar! saved-formbar-insertion-index
                                      (:forms
                                       (get-in (project-attr :formbars)
                                               (formbar-path-at (:down-pos mouse))))))))
            (when (and (not saved-formbar-insertion-index)
                       formbar-placement-path)
              (let [dragged-formbar-path (formbar-path-at (:down-pos mouse))]
                (when (not (or (= formbar-placement-path
                                  dragged-formbar-path)
                               (= formbar-placement-path
                                  (update dragged-formbar-path 2 inc))
                               (in-discard-corner? mouse)))
                  (let [forms (:forms
                               (get-in (project-attr :formbars)
                                       dragged-formbar-path))
                        dragged-formbar (get-in (project-attr :formbars)
                                                dragged-formbar-path)
                        create-new! (fn []
                                      (add-project-formbar-at formbar-placement-path
                                                              dragged-formbar))
                        delete-old! (fn []
                                      (delete-project-formbar-at dragged-formbar-path))]
                    (if (and (= (take 2 formbar-placement-path)
                                (take 2 dragged-formbar-path))
                             (< (nth formbar-placement-path 2)
                                (nth dragged-formbar-path 2)))
                      (do (delete-old!)
                          (create-new!))
                      (do (create-new!)
                          (delete-old!))))))))

          (= (:down-zone mouse) :tool-circle)
          (let [formbar-placement-path (formbar-insertion-path-at mouse)]
            (when formbar-placement-path
              (add-project-formbar-at formbar-placement-path
                                      {:type :tool
                                       :tool-type (tool-circle-at
                                                   (:down-pos mouse))})))

          (= (:down-zone mouse) :saved-formbar)
          (if (= mouse-zone :saved-formbar)
            (let [from-index (+ @saved-formbar-scroll-pos
                                (saved-formbar-index-at (:down-pos mouse)))
                  to-index (+ @saved-formbar-scroll-pos
                              (saved-formbar-insertion-index-at mouse))
                  saved-formbars (saved-formbar-contents)]
              (when (and (< from-index (count saved-formbars))
                         from-index to-index
                         (not= from-index to-index))
                (add-saved-formbar! to-index
                                    (nth saved-formbars (min (dec (count saved-formbars))
                                                             from-index)))
                (delete-saved-formbar! (if (> from-index to-index)
                                         (inc from-index)
                                         from-index))))
            (when (not (in-discard-corner? mouse))
              (let [formbar-placement-path (formbar-insertion-path-at mouse)
                    saved-formbars (saved-formbar-contents)]
                (when formbar-placement-path
                  (let [selected-saved-formbar-index (+ @saved-formbar-scroll-pos
                                                        (saved-formbar-index-at (:down-pos mouse)))
                        saved-formbar (nth saved-formbars selected-saved-formbar-index)]
                    (add-project-formbar-at formbar-placement-path)
                    (doseq [index (range (count saved-formbar))]
                      (let [form (nth saved-formbar index)]
                        (add-project-formbar-form-at form formbar-placement-path index)))))))))
        (case (:down-zone mouse)
          :back-icon
          (enter-page :code)

          :color-scheme
          (set-global-attr! :color-scheme (color-scheme-index-at mouse))

          :new-project
          (do (new-project)
              (refresh-dropdown-names))

          :duplicate-project
          (do (duplicate-project)
              (refresh-dropdown-names))

          :delete-project
          (do (delete-project)
              (refresh-dropdown-names))

          :rename-project
          (activate-project-rename-input)

          nil)))}))