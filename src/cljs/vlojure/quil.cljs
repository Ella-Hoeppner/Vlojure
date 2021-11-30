(ns vlojure.quil
  (:require [vlojure.util :as u]
            [vlojure.storage :refer [project-attr
                                     set-project-attr!]]))

(defonce quil-canvas (atom nil))

(defn activate-quil-mode! []
  (u/log "activating")
  (set-project-attr! :quil true))

(defn deactivate-quil-mode! []
  (u/log "deactivating")
  (set-project-attr! :quil false))

(defn quil-mode? []
  (boolean (project-attr :quil)))

(defn resize-quil-canvas [width height]
  (when (and (quil-mode?)
             @quil-canvas)
    (let [style (.-style @quil-canvas)]
      (set! (.-left style) width)
      (set! (.-top style) 0))
    (set! (.-width @quil-canvas) width)
    (set! (.-height @quil-canvas) height)))

(defn init-quil-canvas []
  (reset! quil-canvas (js/document.createElement "canvas"))
  (set! (.-id @quil-canvas) "quil")
  (set! (.-position (.-style @quil-canvas)) "absolute")
  (js/document.body.appendChild @quil-canvas))