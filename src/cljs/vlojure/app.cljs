(ns vlojure.app
  (:require [vlojure.util :as u]
            [vlojure.constants :as c]
            [vlojure.graphics :refer [app-rect
                                      draw-circle
                                      draw-rect
                                      draw-line
                                      draw-polyline
                                      update-svgs
                                      get-delta
                                      html-color
                                      app-width
                                      app-height
                                      app-size]]
            [vlojure.storage :refer [color-scheme
                                     update-global-attr!
                                     global-attr
                                     base-zoom
                                     project-attr
                                     undo!
                                     redo!
                                     add-project-formbar-at]]
            [vlojure.formbar :refer [formbar-form-path-at
                                     new-formbar-circle-path-at]]
            [vlojure.geometry :refer [add-points
                                      subtract-points
                                      scale-point
                                      unit
                                      TAU
                                      angle-point
                                      point-angle
                                      scalar-point-projection
                                      point-magnitude]]))

;;; This file defines the core logic of the app that is common between the
;;; different pages. The logic specific to the code page, the settings page,
;;; and the text page are defined in the /pages/ directory. This file ties
;;; together the pages and provides functionality for dealing with user input.

(defonce pages (atom {}))
(defonce active-page (atom :code))
(defonce mouse (atom nil))

(defn register-page! [key page]
  (swap! pages
         #(assoc % key page)))

(defn page-action [page action & args]
  (let [action (get-in @pages [page action])]
    (when action
      (apply action args))))

(defn all-pages-action [action & args]
  (doseq [page (keys @pages)]
    (apply page-action (conj args action page))))

(defn enter-page [page]
  (page-action @active-page :exit)
  (page-action page :enter)
  (reset! active-page page))

(defn resize-html-elements []
  (all-pages-action :resize-html))

(defn get-mouse-zone []
  (page-action @active-page
               :mouse-zone
               @mouse))

(defn update-mouse-dragging []
  (swap! mouse
         #(assoc %
                 :dragging?
                 (> (:drag-dist %)
                    c/min-drag-dist))))

(defn render-top-left-button-background [& [highlighted-background?]]
  (let [current-app-rect (app-rect)
        [app-pos] current-app-rect
        background-color (if highlighted-background?
                           (:highlight (color-scheme))
                           (:foreground (color-scheme)))]
    (draw-circle (assoc app-pos
                        :radius c/upper-corner-zone-radius)
                 background-color
                 :menu)))

(defn render-top-left-settings-button [& [highlighted-background?]]
  (let [current-app-rect (app-rect)
        [app-pos] current-app-rect
        radius (/ (* (- 1 c/corner-zone-bar-thickness)
                     c/upper-corner-zone-radius)
                  (inc (Math/sqrt 2)))
        base-circle-pos (add-points app-pos
                                    (scale-point unit radius))
        background-color (if highlighted-background?
                           (:highlight (color-scheme))
                           (:foreground (color-scheme)))]
    (doseq [angle (map (partial * TAU)
                       (u/prop-range c/settings-zone-icon-spokes true))]
      (draw-line base-circle-pos
                 (add-points base-circle-pos
                             (scale-point (angle-point angle)
                                          (* radius
                                             c/settings-zone-icon-spoke-length-factor)))
                 (* radius
                    c/settings-zone-icon-spoke-width-factor)
                 (:text (color-scheme))
                 :menu))
    (draw-circle (assoc base-circle-pos
                        :radius (* radius
                                   c/settings-zone-icon-radius-factor))
                 (:text (color-scheme))
                 :menu)
    (draw-circle (assoc base-circle-pos
                        :radius (* radius
                                   c/settings-zone-icon-radius-factor
                                   c/settings-zone-icon-inner-radius-factor))
                 background-color
                 :menu)))

(defn render-top-left-back-button []
  (let [current-app-rect (app-rect)
        [app-pos] current-app-rect
        radius (/ (* (- 1 c/corner-zone-bar-thickness)
                     c/upper-corner-zone-radius)
                  (inc (Math/sqrt 2)))
        base-circle-pos (add-points app-pos
                                    (scale-point unit radius))
        tip (add-points base-circle-pos
                        {:x (- (* radius
                                  c/back-icon-left-length-factor))})
        width (* radius c/back-icon-width-factor)
        arrow-size (* radius c/back-icon-tip-length-factor)]
    (draw-line tip
               (add-points base-circle-pos
                           {:x (* radius
                                  c/back-icon-right-length-factor)})
               width
               (:text (color-scheme))
               :menu)
    (draw-polyline [(add-points tip
                                {:x arrow-size
                                 :y (- arrow-size)})
                    tip
                    (add-points tip
                                {:x arrow-size
                                 :y arrow-size})]
                   width
                   (:text (color-scheme))
                   :menu)))

(defn render-top-left-invalid-button []
  (let [current-app-rect (app-rect)
        [app-pos] current-app-rect
        radius (/ (* (- 1 c/corner-zone-bar-thickness)
                     c/upper-corner-zone-radius)
                  (inc (Math/sqrt 2)))
        base-circle-pos (add-points app-pos
                                    (scale-point unit radius))
        base-offset (scale-point unit
                                 (* (Math/sqrt 0.5)
                                    c/new-icon-size
                                    radius))]
    (doseq [offset [base-offset (update base-offset :x -)]]
      (draw-line (add-points base-circle-pos
                             (scale-point offset -1))
                 (add-points base-circle-pos
                             offset)
                 (* radius c/new-icon-width)
                 (:text (color-scheme))
                 :menu))))

(defn render-app-state []
  (let [current-app-rect (app-rect)
        mouse-zone (get-mouse-zone)]
    (draw-rect current-app-rect
               (:background (color-scheme))
               :background)
    (page-action @active-page
                 :render
                 @mouse
                 mouse-zone)
    (update-svgs)))

(defn update-app []
  (update-mouse-dragging)
  (let [delta (get-delta)]
    (when-not (zero? c/scroll-speed)
      (update-global-attr! :scroll-direction
                           #(angle-point
                             (+ (point-angle %)
                                (* c/scroll-speed delta)))))
    (when (and (:down? @mouse)
               (:dragging? @mouse)
               (= (:down-zone @mouse) :empty))
      (page-action @active-page :scroll
                   (-
                    (/ (scalar-point-projection (subtract-points @mouse
                                                                 (:last-pos @mouse))
                                                (global-attr :scroll-direction))
                       (* (base-zoom)
                          c/outer-form-spacing)))
                   @mouse))
    (all-pages-action :update delta @mouse (get-mouse-zone))
    (swap! mouse
           #(assoc %
                   :last-pos
                   (select-keys % [:x :y])))
    (resize-html-elements)
    (render-app-state)))

(defn update-mouse-pos [event]
  (let [screen-pos (.-global (.-data event))
        x (.-x screen-pos)
        y (.-y screen-pos)
        width (app-width)
        height (app-height)
        size (app-size)
        current-pos {:x (/ (- x (* 0.5 (- width size))) size)
                     :y (/ (- y (* 0.5 (- height size))) size)}]
    (swap! mouse
           (fn [mouse-state]
             (let [diff (subtract-points mouse-state
                                         current-pos)]
               (assoc (merge mouse-state current-pos)
                      :drag-dist (when (:down? mouse-state)
                                   (+ (point-magnitude diff)
                                      (:drag-dist mouse-state)))))))))

(defn on-click-down [event]
  (update-mouse-pos event)
  (let [zone (get-mouse-zone)]
    (swap! mouse
           (fn [mouse-state]
             (assoc mouse-state
                    :down-pos (select-keys mouse-state [:x :y])
                    :down? true
                    :down-zone zone
                    :down-formbar-form-path (formbar-form-path-at mouse-state))))
    (page-action @active-page :click-down @mouse zone)))

(defn refresh-html-colors []
  (all-pages-action :refresh-html-colors)
  (let [ss (first js/document.styleSheets)]
    (when ss
      (.insertRule ss
                   (str "::selection { background: "
                        (html-color (:background (color-scheme)))
                        "}")))))

(defn on-click-up [event]
  (update-mouse-pos event)
  (update-mouse-dragging)
  (let [mouse-zone (get-mouse-zone)]
    (page-action @active-page
                 :click-up
                 @mouse
                 mouse-zone)
    (swap! mouse
           (fn [mouse-state]
             (assoc mouse-state
                    :down? false
                    :drag-dist 0)))
    (when-not (:dragging? @mouse)
      (case (:down-zone @mouse)
        :formbar
        (let [form-path (take 3 (formbar-form-path-at (:down-pos @mouse)))
              {:keys [tool-type]} (get-in (project-attr :formbars) form-path)]
          (case tool-type
            :undo (undo!)
            :redo (redo!)
            nil))

        :color-scheme
        (refresh-html-colors)

        :text-icon
        (enter-page :text)

        :new-formbar
        (add-project-formbar-at (new-formbar-circle-path-at @mouse))

        nil))))

(defn init []
  (u/log "App Initializing...")

  (doseq [action [:refresh-html-colors :resize-html]]
    (all-pages-action action))

  (enter-page :code)
  (resize-html-elements))
