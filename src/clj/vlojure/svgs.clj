(ns vlojure.svgs
  (:require [clojure.java.io :as java-io]
            [dali.io :as io]))

(defn rgb
  "Takes an integer `color` between 0 and 0xffffff, inclusive, and returns a
   string representing the same color formatted for use in svgs."
  [color]
  (str "#" (format "%06x" color)))


; Constants used to define SVGs
(def PI Math/PI)
(def undo-radius 30)
(def undo-arrow-width 0.8)
(def undo-arrow-length (/ 0.8 (Math/sqrt 2)))
(def white (rgb 0xffffff))
(def output-directory "./generated_svgs/")

; helper functions
(defn path [attributes [start-x start-y] & segments]
  (let [segment-strings (mapv (fn [[type & [arg-map]]]
                                (str
                                 (case type
                                   :arc (let [radius-x (or (:rx arg-map)
                                                           (:r arg-map))
                                              radius-y (or (:rx arg-map)
                                                           (:r arg-map))
                                              rotation (or (:rotation arg-map)
                                                           0)
                                              {:keys [large? sweep? end-pos]} arg-map]
                                          (apply str
                                                 (conj (map #(str % " ")
                                                            (concat [radius-x radius-y
                                                                     rotation
                                                                     (if large? 1 0)
                                                                     (if sweep? 1 0)]
                                                                    end-pos))
                                                       "A "))))
                                 "\n"))
                              segments)]
    [:path (merge attributes
                  {:d (str "M " start-x " " start-y "\n"
                           (apply str segment-strings))})]))

(defn circle-pos [radius angle]
  [(+ 50 (* radius (Math/cos angle)))
   (+ 50 (* radius (Math/sin angle)))])


; "documents" defines all SVGs that will be exported when "export" is run
(def documents
  ; TODO: replace literal-fn-replace enclose vector-enclose fn-enclose let-enclose comment
  (let []
    (mapv
     (fn [[name doc-fragments]]
       [name
        (vec
         (concat [:dali/page
                  {:width 100 :height 100}
                  [:rect {:fill (rgb 0)}
                   [0 0]
                   [100 100]]]
                 doc-fragments))])
     [["undo" [(path {:stroke white
                      :stroke-width 12
                      :stroke-linecap "round"}
                     (circle-pos undo-radius (* 0.5 PI))
                     [:arc
                      {:r undo-radius
                       :large? true
                       :end-pos (circle-pos undo-radius (* 1.2 PI))}])
               [:polygon {:fill white}
                (circle-pos (* (inc undo-arrow-width) undo-radius) (* 1.25 PI))
                (circle-pos (* (- 1 undo-arrow-width) undo-radius) (* 1.25 PI))
                (mapv #(+ %1
                          (* %2 %3))
                      (circle-pos undo-radius (* 1.25 PI))
                      [-1 1]
                      (repeat (* undo-radius undo-arrow-length)))]]]
      ["redo" [(path {:stroke white
                      :stroke-width 12
                      :stroke-linecap "round"}
                     (circle-pos undo-radius (* 1.8 PI))
                     [:arc
                      {:r undo-radius
                       :large? true
                       :end-pos (circle-pos undo-radius (* 0.5 PI))}])
               [:polygon {:fill white}
                (circle-pos (* (inc undo-arrow-width) undo-radius) (* 1.75 PI))
                (circle-pos (* (- 1 undo-arrow-width) undo-radius) (* 1.75 PI))
                (mapv +
                      (circle-pos undo-radius (* 1.75 PI))
                      (repeat (* undo-radius undo-arrow-length)))]]]])))

(defn render-document
  "Takes in a dali `document` and a `filename`, and saves the document as an
   SVG with the given filename"
  [filename document]
  (.delete (java-io/file (str output-directory filename ".svg")))
  (io/render-svg document (str output-directory filename ".svg")))

(defn export
  "Saves all SVGs by calling 'render-document' on all elements of 'documents'"
  []
  (doseq [name-document-pair documents]
    (apply render-document name-document-pair)))

(comment
  (do (require 'vlojure.svgs :reload)
      (export))
  )