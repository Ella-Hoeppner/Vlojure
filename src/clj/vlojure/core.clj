(ns vlojure.core
  (:require [stasis.core :as stasis]
            [ring.adapter.jetty :as ring]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [include-js include-css]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [optimus.prime :as optimus]
            [optimus.assets :as assets]
            [optimus.optimizations :as optimizations]
            [optimus.strategies :refer [serve-live-assets]]
            [optimus.export]))

(defn get-assets []
  (concat (assets/load-assets "public/styles"
                              ["/main.css"
                               "/monoid-bold.ttf"])
          (assets/load-assets "public/js"
                              ["/base.js"
                               "/manifest.edn"])
          (assets/load-assets "public"
                              [#"\/bootstrap\/.*"])))

(def pages
  {"/index.html" (html {:lang "en"}
                       [:head
                        [:meta {:charset "utf-8"}]
                        (include-css "main.css")]
                       [:body
                        [:script {:src "base.js" :type "text/javascript" :charset "utf-8"}]
                        #_(include-js "base.js")])
   "/about/" (html {:lang "en"}
                   [:head]
                   [:body
                    [:p "Coming soon."]])})

(def app
  (-> (stasis/serve-pages pages)
      (optimus/wrap get-assets
                    optimizations/none
                    serve-live-assets)
      wrap-content-type))

(defn start-server [& [port]]
  (ring/run-jetty app
                  {:port (if port
                           (Integer/parseInt port)
                           3000)
                   :join? false}))

(defn export []
  (let [export-dir "out/vlojure"
        optimized-assets (optimizations/none (get-assets) {})]
    (stasis/empty-directory! export-dir)
    (optimus.export/save-assets optimized-assets export-dir)
    (stasis/export-pages pages export-dir {:optimus-assets optimized-assets})))

(defn -main [& [mode]]
  (case mode
    "export" (export)
    (start-server)))