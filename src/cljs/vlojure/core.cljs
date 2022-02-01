(ns vlojure.core
  (:require [vlojure.util :as u]
            [vlojure.graphics :as graphics]
            [vlojure.app :as app]
            [vlojure.storage :as storage]
            [vlojure.evaluation :as evaluation]
            [vlojure.pages.code :as code-page]
            [vlojure.pages.settings :as settings-page]
            [vlojure.layout :as layout]
            [vlojure.pages.text :as text-page]))

(defn init []
  (u/log "Initializing...")
  (storage/init)
  (app/init)
  (code-page/init)
  (settings-page/init)
  (text-page/init)
  (graphics/init (fn []
                   (graphics/update-graphics)
                   (app/update-app))
                 app/on-click-down
                 app/on-click-up
                 app/update-mouse-pos)
  (app/refresh-html-colors)
  (evaluation/init))