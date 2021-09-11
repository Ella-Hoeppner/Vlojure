(ns vlojure.core
  (:require [vlojure.graphics :as graphics]
            [vlojure.app :as app]
            [vlojure.storage :as storage]
            [vlojure.evaluation :as evaluation]))

(defn init []
  (js/console.log "Initializing...")
  (storage/init)
  (app/init)
  (graphics/init
   (fn []
     (graphics/update-graphics)
     (app/update-app))
   app/on-click-down
   app/on-click-up
   app/update-mouse-pos)
  (evaluation/init))