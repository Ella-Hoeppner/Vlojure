(ns vlojure.pages.text
  (:require [vlojure.util :as u]
            [vlojure.graphics :refer [app-rect
                                      draw-rect
                                      screen-x
                                      screen-y
                                      html-color]]
            [vlojure.storage :as storage]
            [vlojure.constants :as c]
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
    (set! (.-fontFamily style) c/font-name)
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
            left-x (screen-x (+ (:x (first (app-rect)))
                                         c/text-page-border))
            top-y (screen-y (+ (:y (first (app-rect)))
                                        c/text-page-border))]
        (set! (.-left (.-style input))
              (str left-x
                   "px"))
        (set! (.-width (.-style input))
              (str (- (screen-x (- (:x (apply geom/add-points (app-rect)))
                                            c/text-page-border))
                      top-y)
                   "px"))
        (set! (.-top (.-style input))
              (str top-y
                   "px"))
        (set! (.-height (.-style input))
              (str (- (screen-y (- (:y (apply geom/add-points (app-rect)))
                                            c/text-page-border))
                      top-y)
                   "px"))))

    :refresh-html-colors
    (fn []
      (set! (.-color (.-style @input-element))
            (html-color (:text (storage/color-scheme)))))

    :mouse-zone
    (fn [mouse]
      (cond
        (<= (geom/point-magnitude
             (geom/subtract-points (first (app-rect))
                                   mouse))
            c/upper-corner-zone-radius)
        :back-icon

        :else :empty))

    :render
    (fn [mouse mouse-zone]
      (draw-rect (let [[app-pos app-size] (app-rect)]
                            [(reduce #(update %1 %2 (partial + c/text-page-border))
                                     app-pos
                                     [:x :y])
                             (reduce #(update %1 %2 (fn [v] (- v (* 2 c/text-page-border))))
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