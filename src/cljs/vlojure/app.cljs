(ns vlojure.app
  (:require [clojure.string :as string]
            [vlojure.graphics :as graphics]
            [vlojure.storage :as storage]
            [vlojure.formbar :as formbar]
            [vlojure.layout :as layout]
            [vlojure.util :as u]
            [vlojure.geometry :as geom]
            [vlojure.constants :as constants]
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
  (> (:drag-dist (attr :mouse)) constants/min-drag-dist))

(defn render-top-left-button-background [& [highlighted-background?]]
  (let [current-app-rect (graphics/app-rect)
        [app-pos] current-app-rect
        background-color (if highlighted-background?
                           (:highlight (storage/color-scheme))
                           (:foreground (storage/color-scheme)))]
    (graphics/circle (assoc app-pos
                            :radius constants/upper-corner-zone-radius)
                     background-color
                     :menu)))

(defn render-top-left-settings-button [& [highlighted-background?]]
  (let [current-app-rect (graphics/app-rect)
        [app-pos] current-app-rect
        radius (/ (* (- 1 constants/corner-zone-bar-thickness)
                     constants/upper-corner-zone-radius)
                  (inc (Math/sqrt 2)))
        base-circle-pos (geom/add-points app-pos
                                         (geom/scale-point geom/unit radius))
        background-color (if highlighted-background?
                           (:highlight (storage/color-scheme))
                           (:foreground (storage/color-scheme)))]
    (doseq [angle (map (partial * geom/TAU)
                       (u/prop-range constants/settings-zone-icon-spokes true))]
      (graphics/line base-circle-pos
                     (geom/add-points base-circle-pos
                                      (geom/scale-point (geom/angle-point angle)
                                                        (* radius
                                                           constants/settings-zone-icon-spoke-length-factor)))
                     (* radius
                        constants/settings-zone-icon-spoke-width-factor)
                     (:text (storage/color-scheme))
                     :menu))
    (graphics/circle (assoc base-circle-pos
                            :radius (* radius
                                       constants/settings-zone-icon-radius-factor))
                     (:text (storage/color-scheme))
                     :menu)
    (graphics/circle (assoc base-circle-pos
                            :radius (* radius
                                       constants/settings-zone-icon-radius-factor
                                       constants/settings-zone-icon-inner-radius-factor))
                     background-color
                     :menu)))

(defn render-top-left-back-button []
  (let [current-app-rect (graphics/app-rect)
        [app-pos] current-app-rect
        radius (/ (* (- 1 constants/corner-zone-bar-thickness)
                     constants/upper-corner-zone-radius)
                  (inc (Math/sqrt 2)))
        base-circle-pos (geom/add-points app-pos
                                         (geom/scale-point geom/unit radius))
        tip (geom/add-points base-circle-pos
                             {:x (- (* radius
                                       constants/back-icon-left-length-factor))})
        width (* radius constants/back-icon-width-factor)
        arrow-size (* radius constants/back-icon-tip-length-factor)]
    (graphics/line tip
                   (geom/add-points base-circle-pos
                                    {:x (* radius
                                           constants/back-icon-right-length-factor)})
                   width
                   (:text (storage/color-scheme))
                   :menu)
    (graphics/polyline [(geom/add-points tip
                                         {:x arrow-size
                                          :y (- arrow-size)})
                        tip
                        (geom/add-points tip
                                         {:x arrow-size
                                          :y arrow-size})]
                       width
                       (:text (storage/color-scheme))
                       :menu)))

(defn render-top-left-invalid-button []
  (let [current-app-rect (graphics/app-rect)
        [app-pos] current-app-rect
        radius (/ (* (- 1 constants/corner-zone-bar-thickness)
                     constants/upper-corner-zone-radius)
                  (inc (Math/sqrt 2)))
        base-circle-pos (geom/add-points app-pos
                                         (geom/scale-point geom/unit radius))
        base-offset (geom/scale-point geom/unit
                                      (* (Math/sqrt 0.5)
                                         constants/new-icon-size
                                         radius))]
    (doseq [offset [base-offset (update base-offset :x -)]]
      (graphics/line (geom/add-points base-circle-pos
                                      (geom/scale-point offset -1))
                     (geom/add-points base-circle-pos
                                      offset)
                     (* radius constants/new-icon-width)
                     (:text (storage/color-scheme))
                     :menu))))

(defn render-app-state []
  (let [current-app-rect (graphics/app-rect)
        mouse-zone (get-mouse-zone)]
    (graphics/rect current-app-rect
                   (:background (storage/color-scheme))
                   :background)
    (page-action (attr :page)
                 :render
                 (assoc (attr :mouse)
                        :dragging? (mouse-dragging?))
                 mouse-zone)
    (graphics/update-svgs)))

(defn update-app []
  (let [delta (graphics/get-delta)]
    (when-not (zero? constants/scroll-speed)
      (storage/update-attr! :scroll-direction
                            #(geom/angle-point
                              (+ (geom/point-angle %)
                                 (* constants/scroll-speed delta)))))
    (let [{:keys [mouse page]} @app-state]
      (when (and (:down? mouse)
                 (mouse-dragging?)
                 (= (:down-zone mouse) :empty))
        (page-action page :scroll
                     (-
                      (/ (geom/scalar-point-projection (geom/subtract-points mouse
                                                                             (:last-pos mouse))
                                                       (storage/attr :scroll-direction))
                         (* (storage/base-zoom)
                            constants/outer-form-spacing)))
                     (assoc mouse :dragging? (mouse-dragging?)))))
    (prn (get-mouse-zone))
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
        width (graphics/app-width)
        height (graphics/app-height)
        size (graphics/app-size)
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
                        (graphics/html-color (:background (storage/color-scheme)))
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
              {:keys [tool-type]} (get-in (storage/project-attr :formbars) form-path)]
          (case tool-type
            :undo (storage/undo!)
            :redo (storage/redo!)
            nil))

        :color-scheme
        (refresh-html-colors)

        :text-icon
        (enter-page :text)

        :new-formbar
        (storage/add-project-formbar-at (formbar/new-formbar-circle-path-at mouse))

        nil))))

(defn init []
  (u/log "App Initializing...")
  (set-attr! :page :code)

  (doseq [action [:refresh-html-colors :resize-html]]
    (all-pages-action action))

  (refresh-html-colors)

  (enter-page :code)
  (resize-html-elements))
