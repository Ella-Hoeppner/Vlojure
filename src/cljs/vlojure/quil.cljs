(ns vlojure.quil
  (:require [vlojure.util :as u]
            [vlojure.storage :refer [project-attr
                                     set-project-attr!]]
            [quil.core :as q :include-macros true]
            [vlojure.evaluation :refer [eval-clj once-eval-ready]]))

(defonce quil-div (atom nil))

(defn activate-quil-mode! []
  (set-project-attr! :quil true)
  (when @quil-div
    (set! (.-visibility (.-style @quil-div)) "visible")))

(defn deactivate-quil-mode! []
  (set-project-attr! :quil false)
  (when @quil-div
    (set! (.-visibility (.-style @quil-div)) "hidden")))

(defn quil-mode? []
  (boolean (project-attr :quil)))

(defn resize-quil [width height]
  (when (and (quil-mode?)
             @quil-div)
    (let [style (.-style @quil-div)]
      (set! (.-left style) width)
      (set! (.-top style) 0))
    (set! (.-width @quil-div) width)
    (set! (.-height @quil-div) height)))

(defn load-namespaces []
  (eval-clj (str
             '(do (require '[quil.core :as q])
                  (require '[vlojure.quil :refer [start-sketch!]])))
            #(u/log "Quil initialization success" %)
            #(u/log "Quil initialization failure" %)))

(defn init-quil []
  (reset! quil-div (js/document.createElement "div"))
  (set! (.-id @quil-div) "quil")
  (set! (.-position (.-style @quil-div)) "absolute")
  (js/document.body.appendChild @quil-div)
  (once-eval-ready load-namespaces))

(defn start-sketch! [draw-fn]
  (q/sketch
   :host "quil"
   :size [(.-width @quil-div) (.-height @quil-div)]
   :draw #(draw-fn (.-width @quil-div) (.-height @quil-div))))

(defn test-draw [w h]
  (q/background 0 0 0)
  (q/no-stroke)
  (q/fill 255 0 0)
  (q/rect 0 0 (* w 0.5) (* h 0.5)))
