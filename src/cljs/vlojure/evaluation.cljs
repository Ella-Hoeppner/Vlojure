(ns vlojure.evaluation
  (:require [vlojure.errors :refer [log-error! clear-error!]])
  (:require [cljs.js :refer [empty-state js-eval eval-str]]
            [shadow.cljs.bootstrap.browser :as shadow.bootstrap]))

(defonce c-state (empty-state))
(defonce eval-ready? (atom false))
(defonce on-ready-functions (atom []))

(defn eval-clj [source success-callback failure-callback]
  (clear-error!)
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
              options
              f)))

(defn init []
  (let [old-error-fn js/console.error]
    (set! (.-error js/console)
          (fn [error]
            (log-error! error)
            (old-error-fn error))))
  (shadow.bootstrap/init c-state
                         {:path "/bootstrap"
                          :load-on-init '#{vlojure.quil quil.core}}
                         #(do (reset! eval-ready? true)
                              (doseq [f @on-ready-functions] (f)))))

(defn once-eval-ready [f]
  (if @eval-ready?
    (f)
    (swap! on-ready-functions conj f)))