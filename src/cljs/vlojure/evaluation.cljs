(ns vlojure.evaluation
  (:require [cljs.js :as cljs]
            [cljs.env :as env]
            [shadow.cljs.bootstrap.browser :as shadow.bootstrap]))

(defonce c-state (cljs/empty-state))
(defonce !eval-ready? (atom false))

(defn eval-clj [source success-callback failure-callback]
  (let [options {:eval cljs/js-eval
                 :load (partial shadow.bootstrap/load c-state)
                 :context :expr}
        f (fn [x]
            (if (:error x)
              (failure-callback (:error x))
              (success-callback (:value x))))]
    (cljs/eval-str c-state
                   (str source)
                   nil
                   options f)))

(defn init []
  (shadow.bootstrap/init c-state
                         {:path "/bootstrap"
                          :load-on-init '#{vlojure.user}}
                         #(reset! !eval-ready? true)))