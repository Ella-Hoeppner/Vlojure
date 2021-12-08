(ns vlojure.errors
  (:require [vlojure.util :as u])
  (:require [clojure.string :as string]))

(defonce error-log (atom nil))

(def log-error! (partial reset! error-log))

(defn clear-error! [] (reset! error-log nil))

(defn logged-error []
  (when @error-log
    (string/replace @error-log
                    #" at line \d $"
                    "")))