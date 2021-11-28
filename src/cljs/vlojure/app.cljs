(ns vlojure.app
  (:require [clojure.string :as string]
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
            [vlojure.formbar :as formbar]
            [vlojure.layout :as layout]
            [vlojure.util :as u]
            [vlojure.geometry :as geom]
            [vlojure.constants :as c]
            [vlojure.vedn :as vedn]
            [vlojure.evaluation :as evaluation]))

;;; This file defines the core logic of the app that is common between the
;;; different pages. The logic specific to the code page, the settings page,
;;; and the text page are defined in the /pages/ directory. This file ties
;;; together the pages and provides functionality for dealing with user input.

(defonce pages (atom {}))

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

(defonce app-state (atom {}))

(defn attr [key]
  (get @app-state key))

(defn set-attr! [key value]
  (swap! app-state
         #(assoc % key value))
  value)

(defn update-attr! [key value]
  (swap! app-state
         #(update % key value)))

(defn enter-page [page]
  (let [last-page (attr :page)]
    (page-action last-page :exit))
  (page-action page :enter)
  (set-attr! :page page))

(defn resize-html-elements []
  (all-pages-action :resize-html))

(defn get-mouse-zone []
  (page-action (attr :page)
               :mouse-zone
               (:mouse @app-state)))

(defn mouse-dragging? []
  (> (:drag-dist (attr :mouse)) c/min-drag-dist))

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
        base-circle-pos (geom/add-points app-pos
                                         (geom/scale-point geom/unit radius))
        background-color (if highlighted-background?
                           (:highlight (color-scheme))
                           (:foreground (color-scheme)))]
    (doseq [angle (map (partial * geom/TAU)
                       (u/prop-range c/settings-zone-icon-spokes true))]
      (draw-line base-circle-pos
                 (geom/add-points base-circle-pos
                                  (geom/scale-point (geom/angle-point angle)
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
        base-circle-pos (geom/add-points app-pos
                                         (geom/scale-point geom/unit radius))
        tip (geom/add-points base-circle-pos
                             {:x (- (* radius
                                       c/back-icon-left-length-factor))})
        width (* radius c/back-icon-width-factor)
        arrow-size (* radius c/back-icon-tip-length-factor)]
    (draw-line tip
                   (geom/add-points base-circle-pos
                                    {:x (* radius
                                           c/back-icon-right-length-factor)})
                   width
                   (:text (color-scheme))
                   :menu)
    (draw-polyline [(geom/add-points tip
                                         {:x arrow-size
                                          :y (- arrow-size)})
                        tip
                        (geom/add-points tip
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
        base-circle-pos (geom/add-points app-pos
                                         (geom/scale-point geom/unit radius))
        base-offset (geom/scale-point geom/unit
                                      (* (Math/sqrt 0.5)
                                         c/new-icon-size
                                         radius))]
    (doseq [offset [base-offset (update base-offset :x -)]]
      (draw-line (geom/add-points base-circle-pos
                                      (geom/scale-point offset -1))
                     (geom/add-points base-circle-pos
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
    (page-action (attr :page)
                 :render
                 (assoc (attr :mouse)
                        :dragging? (mouse-dragging?))
                 mouse-zone)
    (update-svgs)))

(defn update-app []
  (let [delta (get-delta)]
    (when-not (zero? c/scroll-speed)
      (update-global-attr! :scroll-direction
                                   #(geom/angle-point
                                     (+ (geom/point-angle %)
                                        (* c/scroll-speed delta)))))
    (let [{:keys [mouse page]} @app-state]
      (when (and (:down? mouse)
                 (mouse-dragging?)
                 (= (:down-zone mouse) :empty))
        (page-action page :scroll
                     (-
                      (/ (geom/scalar-point-projection (geom/subtract-points mouse
                                                                             (:last-pos mouse))
                                                       (global-attr :scroll-direction))
                         (* (base-zoom)
                            c/outer-form-spacing)))
                     (assoc mouse :dragging? (mouse-dragging?)))))
    (all-pages-action :update delta (attr :mouse) (get-mouse-zone))
    (update-attr! :mouse
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
    (update-attr! :mouse
                  (fn [state]
                    (let [diff (geom/subtract-points state
                                                     current-pos)]
                      (assoc (merge state current-pos)
                             :drag-dist (when (:down? state)
                                          (+ (geom/point-magnitude diff)
                                             (:drag-dist state)))))))))

(defn on-click-down [event]
  (update-mouse-pos event)
  (let [{:keys [mouse]} @app-state
        zone (get-mouse-zone)]
    (update-attr! :mouse
                  (fn [state]
                    (assoc state
                           :down-pos (select-keys mouse [:x :y])
                           :down? true
                           :down-zone zone
                           :down-formbar-form-path (formbar/formbar-form-path-at mouse))))
    (page-action (attr :page) :click-down mouse zone)))

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
  (let [{:keys [mouse]} @app-state
        currently-dragging? (mouse-dragging?)
        mouse-zone (get-mouse-zone)]
    (page-action (attr :page)
                 :click-up
                 (assoc mouse
                        :dragging? (mouse-dragging?))
                 mouse-zone)
    (update-attr! :mouse
                  (fn [state]
                    (assoc state
                           :down? false
                           :drag-dist 0)))
    (when-not currently-dragging?
      (case (:down-zone mouse)
        :formbar
        (let [form-path (take 3 (formbar/formbar-form-path-at (:down-pos mouse)))
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
        (add-project-formbar-at (formbar/new-formbar-circle-path-at mouse))

        nil))))

(defn init []
  (u/log "App Initializing...")
  (set-attr! :page :code)

  (doseq [action [:refresh-html-colors :resize-html]]
    (all-pages-action action))

  (enter-page :code)
  (resize-html-elements))
