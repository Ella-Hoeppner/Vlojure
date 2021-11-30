(ns vlojure.evaluation
  (:require [cljs.js :refer [empty-state js-eval eval-str]]
            [shadow.cljs.bootstrap.browser :as shadow.bootstrap]))

(defonce c-state (empty-state))
(defonce !eval-ready? (atom false))

(defn eval-clj [source success-callback failure-callback]
  (let [options {:eval js-eval
                 :load (partial shadow.bootstrap/load c-state)
                 :context :expr}
        f (fn [x]
            (if (:error x)
              (failure-callback (:error x))
              (success-callback (:value x))))]
    (eval-str c-state
              (str source)
              nil
              options f)))

(defn init []
  (shadow.bootstrap/init c-state
                         {:path "/bootstrap"
                          :load-on-init '#{vlojure.user}}
                         #(reset! !eval-ready? true)))