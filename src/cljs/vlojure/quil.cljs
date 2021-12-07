(ns vlojure.quil
  (:require [vlojure.util :as u]
            [vlojure.storage :refer [project-attr
                                     set-project-attr!]]
            [quil.core :as q :include-macros true]
            [vlojure.evaluation :refer [eval-clj once-eval-ready]]))

(defonce quil-div (atom nil))
(defonce quil-canvas-size (atom nil))

(defn activate-quil-mode! []
  (set-project-attr! :quil true)
  (when @quil-div
    (set! (.-visibility (.-style @quil-div)) "visible")))

(defn deactivate-quil-mode! []
  (set-project-attr! :quil false)
  (when @quil-div
    (set! (.-visibility (.-style @quil-div)) "hidden"))
  (while (.-firstChild @quil-div)
    (.removeChild @quil-div
                  (.-firstChild @quil-div))))

(defn quil-mode? []
  (boolean (project-attr :quil)))

(defn quil-width []
  (first @quil-canvas-size))

(defn resize-quil [width height]
  (when (and (quil-mode?)
             @quil-div)
    (let [style (.-style @quil-div)]
      (set! (.-left style) width)
      (set! (.-top style) 0)
      (set! (.-width style) (first @quil-canvas-size))
      (set! (.-height style) height))
    (let [canvas-style (.-style (.-firstChild @quil-div))]
      (set! (.-position canvas-style) "relative")
      (set! (.-top canvas-style) "50%")
      (set! (.-transform canvas-style) "translateY(-50%)"))))

(defn load-namespaces []
  (eval-clj (str
             '(do (require '[quil.core :as q])
                  (require '[vlojure.quil :refer [start-quil!
                                                  stop-quil!]])))
            #(u/log "Quil initialization success" %)
            #(u/log "Quil initialization failure" %)))

(defn init-quil []
  (reset! quil-div (js/document.createElement "div"))
  (set! (.-id @quil-div) "quil")
  (let [style ^js/CSS2Properties (.-style @quil-div)]
    (set! (.-position style) "absolute")
    (set! (.-position style) "absolute")
    (set! (.-display style) "inline-flex"))
  (js/document.body.appendChild @quil-div)
  (once-eval-ready load-namespaces)
  (.addEventListener js/window "keydown"
                     (fn [event]
                       (when (= "F11" (.-key event))
                         (.stopImmediatePropagation event))))
  (deactivate-quil-mode!))

(defn start-quil! [size draw-fn]
  (reset! quil-canvas-size size)
  (activate-quil-mode!)
  (q/sketch
   :host "quil"
   :size size
   :draw #(draw-fn)))

(def stop-quil! deactivate-quil-mode!)
