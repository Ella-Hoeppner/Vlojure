(ns vlojure.pages.text
  (:require [vlojure.util :as u]
            [vlojure.graphics :as graphics]
            [vlojure.storage :as storage]
            [vlojure.constants :as constants]
            [vlojure.geometry :as geom]
            [vlojure.vedn :as vedn]
            [vlojure.app :as app]))

;;; This file contains the logic for the "text" page. This page shows the user
;;; the code for their current project in text form, rather than the visual
;;; form that is normally displayed in the main "code" page. Here the player
;;; can edit their code in text form, or copy the code to move it to an
;;; external location.

(defonce input-element (atom nil))
(defonce current-text-validity (atom true))



(defn text-valid? []
  @current-text-validity)

(defn update-validity! [& _]
  (try (let [current-text (.-value @input-element)]
         (storage/set-project-attr! :form (vedn/clj->vedn current-text))
         (reset! current-text-validity true))
       (catch :default _
         (reset! current-text-validity false))))



(defn init []
  (u/log "Text Page Initializing...")

  (let [input (.createElement js/document "textarea")
        style (.-style input)]
    (reset! input-element input)
    (set! (.-type input) "text")
    (.appendChild (.-body js/document) input)
    (set! (.-onchange input) update-validity!)
    (set! (.-resize style) "none")
    (set! (.-position style) "absolute")
    (set! (.-fontFamily style) constants/font-name)
    (set! (.-background style) "transparent")
    (set! (.-border style) "none")
    (set! (.-outline style) "none")
    (set! (.-display style) "none"))

  (app/register-page!
   :text
   {:enter
    (fn []
      (let [input @input-element]
        (set! (.-display (.-style input)) "block")
        (set! (.-value input)
              (let [form (storage/project-attr :form)]
                (apply str
                       (mapcat (fn [subform]
                                 (str (vedn/vedn->clj subform)
                                      "\n\n"))
                               (:children form))))))
      (update-validity!))

    :exit
    (fn []
      (let [input @input-element]
        (set! (.-display (.-style input)) "none"))
      (storage/fill-empty-project))

    :resize-html
    (fn []
      (let [input @input-element
            left-x (graphics/screen-x (+ (:x (first (graphics/app-rect)))
                                         constants/text-page-border))
            top-y (graphics/screen-y (+ (:y (first (graphics/app-rect)))
                                        constants/text-page-border))]
        (set! (.-left (.-style input))
              (str left-x
                   "px"))
        (set! (.-width (.-style input))
              (str (- (graphics/screen-x (- (:x (apply geom/add-points (graphics/app-rect)))
                                            constants/text-page-border))
                      top-y)
                   "px"))
        (set! (.-top (.-style input))
              (str top-y
                   "px"))
        (set! (.-height (.-style input))
              (str (- (graphics/screen-y (- (:y (apply geom/add-points (graphics/app-rect)))
                                            constants/text-page-border))
                      top-y)
                   "px"))))

    :refresh-html-colors
    (fn []
      (set! (.-color (.-style @input-element))
            (graphics/html-color (:text (storage/color-scheme)))))

    :mouse-zone
    (fn [mouse]
      (cond
        (<= (geom/point-magnitude
             (geom/subtract-points (first (graphics/app-rect))
                                   mouse))
            constants/upper-corner-zone-radius)
        :back-icon

        :else :empty))

    :render
    (fn [mouse mouse-zone]
      (graphics/rect (let [[app-pos app-size] (graphics/app-rect)]
                       [(reduce #(update %1 %2 (partial + constants/text-page-border))
                                app-pos
                                [:x :y])
                        (reduce #(update %1 %2 (fn [v] (- v (* 2 constants/text-page-border))))
                                app-size
                                [:x :y])])
                     (:foreground (storage/color-scheme))
                     :background)


      (app/render-top-left-button-background (= mouse-zone :back-icon))
      (if (text-valid?)
        (app/render-top-left-back-button)
        (app/render-top-left-invalid-button)))

    :click-up
    (fn [mouse mouse-zone]
      (case mouse-zone
        :back-icon
        (when (text-valid?)
          (app/enter-page :code))

        nil))}))