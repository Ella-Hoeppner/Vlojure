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
            [vlojure.evaluation :as evaluation]
            [vlojure.pages.text :as text-page]
            [vlojure.pages.settings :as settings-page]
            [vlojure.pages.code :as code-page]))

;;; This file defines the core logic of the app that is common between the
;;; different pages. The logic specific to the code page, the settings page,
;;; and the text page are defined in the /pages/ directory. This file ties
;;; together the pages and provides functionality for dealing with user input.

(def pages
  {:text text-page/page
   :settings settings-page/page
   :code code-page/page})

(defn page-action [page action & args]
  (let [action (get-in pages [page action])]
    (when action
      (apply action args))))

(defn all-pages-action [action & args]
  (doseq [page (keys pages)]
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

(defn render-app-state []
  (let [current-app-rect (graphics/app-rect)
        [app-pos app-size] current-app-rect
        mouse-zone (get-mouse-zone)]
    (graphics/rect current-app-rect
                   (:background (storage/color-scheme))
                   :background)
    (page-action (attr :page)
                 :render
                 (assoc (attr :mouse)
                        :dragging? (mouse-dragging?))
                 mouse-zone)

    ;; Draw "settings" circle and icon, or "back" icon
    (let [radius (/ (* (- 1 constants/corner-zone-bar-thickness)
                       constants/upper-corner-zone-radius)
                    (inc (Math/sqrt 2)))
          base-circle-pos (geom/add-points app-pos
                                           (geom/scale-point geom/unit radius))
          text-page-valid? (text-page/text-valid?)
          background-color (if (or (not text-page-valid?)
                                   (#{:settings-icon :back-icon} mouse-zone))
                             (:highlight (storage/color-scheme))
                             (:foreground (storage/color-scheme)))]
      (graphics/circle (assoc app-pos
                              :radius constants/upper-corner-zone-radius)
                       background-color
                       :menu)
      (if (= (attr :page) :code)
        (do (doseq [angle (map (partial * geom/TAU)
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
                             :menu))
        (if text-page-valid?
          (let [tip (geom/add-points base-circle-pos
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
                               :menu))
          (let [base-offset (geom/scale-point geom/unit
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
                             :menu))))))))

(defn update-app []
  (let [delta (graphics/get-delta)]
    (when-not (zero? constants/scroll-speed)
      (storage/update-attr! :scroll-direction
                            #(geom/angle-point
                              (+ (geom/point-angle %)
                                 (* constants/scroll-speed delta)))))
    (let [{:keys [mouse page]} @app-state]
      (when (:down? mouse)
        (when (= (:down-zone mouse) :settings-slider)
          (let [settings-circle (settings-page/settings-circle constants/settings-sliders-page)
                x-off (apply - (map :x [mouse settings-circle]))
                adjusted-x-off (/ x-off (* (:radius settings-circle) constants/settings-slider-width))]
            (storage/set-attr! (second
                                (nth constants/settings-sliders
                                     (:down-settings-slider mouse)))
                               (min 1
                                    (max 0
                                         (* 0.5
                                            (inc adjusted-x-off)))))))
        (when (= (:down-zone mouse) :scroll-circle)
          (storage/set-attr! :scroll-direction
                             (geom/angle-point
                              (let [raw-angle (mod (geom/point-angle
                                                    (geom/subtract-points mouse
                                                                          (settings-page/settings-circle constants/settings-project-selector-page)))
                                                   geom/TAU)
                                    snap-angle (some (fn [angle]
                                                       (when (< (min (Math/abs (- angle raw-angle))
                                                                     (- geom/TAU
                                                                        (Math/abs (- angle raw-angle))))
                                                                constants/scroll-angle-snap-distance)
                                                         angle))
                                                     (mapv (partial * geom/TAU)
                                                           (u/prop-range constants/scroll-angle-snap-positions true)))]
                                (or snap-angle
                                    raw-angle)))))
        (when (and (mouse-dragging?)
                   (= (:down-zone mouse) :empty))
          (page-action page :scroll
                       (-
                        (/ (geom/scalar-point-projection (geom/subtract-points mouse
                                                                               (:last-pos mouse))
                                                         (storage/attr :scroll-direction))
                           (* (storage/base-zoom)
                              constants/outer-form-spacing)))
                       (assoc mouse :dragging? (mouse-dragging?))))))
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
        layout (code-page/adjusted-form-layouts)
        layout-path (code-page/layout-path-at layout mouse)
        zone (get-mouse-zone)
        settings-slider (settings-page/settings-slider-at mouse)]
    (update-attr! :mouse
                  (fn [state]
                    (assoc state
                           :down-pos (select-keys mouse [:x :y])
                           :down? true
                           :down-path (vec layout-path)
                           :down-zone zone
                           :down-formbar-form-path (formbar/formbar-form-path-at mouse)
                           :down-settings-slider settings-slider)))
    (page-action :click-down mouse)
    (when (= zone :settings-circle)
      (settings-page/set-ideal-scroll-pos! (settings-page/settings-circle-at (attr :mouse))))))

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
        currently-dragging? (mouse-dragging?)]
    (page-action (attr :page)
                 :click-up
                 (assoc mouse :dragging? (mouse-dragging?))
                 (get-mouse-zone))
    (update-attr! :mouse
                  (fn [state]
                    (assoc state
                           :down? false
                           :drag-dist 0)))
    (if currently-dragging?
      (when (and (= (attr :page) :settings)
                 (#{:formbar} (:down-zone mouse)))
        (let [formbar-placement-path (formbar/formbar-insertion-path-at mouse)]
          (when formbar-placement-path
            (let [dragged-formbar-path (formbar/formbar-path-at (:down-pos mouse))]
              (when (not (or (= formbar-placement-path
                                dragged-formbar-path)
                             (= formbar-placement-path
                                (update dragged-formbar-path 2 inc))
                             (graphics/in-discard-corner? mouse)))
                (let [forms (:forms
                             (get-in (storage/project-attr :formbars)
                                     dragged-formbar-path))
                      create-new! (fn []
                                    (storage/add-project-formbar-at formbar-placement-path)
                                    (doseq [index (range (count forms))]
                                      (let [form (nth forms index)]
                                        (storage/add-project-formbar-form-at form formbar-placement-path index))))
                      delete-old! (fn []
                                    (storage/delete-project-formbar-at dragged-formbar-path))]
                  (if (and (= (take 2 formbar-placement-path)
                              (take 2 dragged-formbar-path))
                           (< (nth formbar-placement-path 2)
                              (nth dragged-formbar-path 2)))
                    (do (delete-old!)
                        (create-new!))
                    (do (create-new!)
                        (delete-old!)))))))))
      (case (:down-zone mouse)
        :settings-icon
        (enter-page (case (attr :page)
                      :code :settings
                      :code))

        :back-icon
        (when (text-page/text-valid?)
          (enter-page :code))

        :color-scheme
        (do (storage/set-attr! :color-scheme (settings-page/color-scheme-index-at mouse))
            (refresh-html-colors))

        :text-icon
        (enter-page :text)

        :new-formbar
        (storage/add-project-formbar-at (formbar/new-formbar-circle-path-at mouse))

        :new-project
        (do (storage/new-project)
            (settings-page/refresh-dropdown-names))

        :duplicate-project
        (do (storage/duplicate-project)
            (settings-page/refresh-dropdown-names))

        :delete-project
        (do (storage/delete-project)
            (settings-page/refresh-dropdown-names))

        :rename-project
        (settings-page/activate-project-rename-input)

        nil))))

(defn init []
  (set-attr! :page :code)

  (doseq [action [:init :refresh-html-colors :resize-html]]
    (all-pages-action action))

  (refresh-html-colors)

  (enter-page :code)
  (resize-html-elements))