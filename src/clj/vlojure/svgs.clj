(ns vlojure.svgs
  (:require [clojure.java.io :as java-io]
            [dali.io :as io]
            [vlojure.constants :as c]))

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
(def foreground-color (rgb 0xff0000))
(def highlight-color (rgb 0x00ff00))
(def text-color (rgb 0x0000ff))
(def output-directory "./resources/public/svgs/")

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

(defn outline-circle [args pos radius outline-factor]
  (let [stroke-width (* radius outline-factor)]
    [:circle (merge args
                    {:fill "none"
                     :stroke-width (* 2 stroke-width)
                     :class "foreground"})
     pos
     (- radius stroke-width)]))

(defn text [[x y] font-size text]
  [:text {:x x :y y
          :fill text-color
          :font-family "monoid-bold"
          :dominant-baseline "middle"
          :text-anchor "middle"
          :font-size font-size}
   text])


; "documents" defines all SVGs that will be exported when "export" is run
(def documents
  ; TODO: enclose vector-enclose fn-enclose let-enclose comment
  (let []
    (mapv
     (fn [[name doc-fragments]]
       [name
        (vec
         (concat [:dali/page
                  {:width 100 :height 100}
                  [:defs
                   [:style {:type "text/css"}
                    "@font-face {
                                  font-family: monoid-bold;
                                  src: url('../styles/monoid-bold.ttf');
                                }"]]]
                 doc-fragments))])
     [["undo"
       [(path {:stroke highlight-color
               :class "highlight"
               :stroke-width 12
               :stroke-linecap "round"
               :fill "none"}
              (circle-pos undo-radius (* 0.5 PI))
              [:arc
               {:r undo-radius
                :large? true
                :end-pos (circle-pos undo-radius (* 1.2 PI))}])
        [:polygon {:fill highlight-color
                   :class "highlight"}
         (circle-pos (* (inc undo-arrow-width) undo-radius) (* 1.25 PI))
         (circle-pos (* (- 1 undo-arrow-width) undo-radius) (* 1.25 PI))
         (mapv #(+ %1
                   (* %2 %3))
               (circle-pos undo-radius (* 1.25 PI))
               [-1 1]
               (repeat (* undo-radius undo-arrow-length)))]]]
      ["redo"
       [(path {:stroke highlight-color
               :class "highlight"
               :stroke-width 12
               :stroke-linecap "round"
               :fill "none"}
              (circle-pos undo-radius (* 1.8 PI))
              [:arc
               {:r undo-radius
                :large? true
                :end-pos (circle-pos undo-radius (* 0.5 PI))}])
        [:polygon {:fill highlight-color
                   :class "highlight"}
         (circle-pos (* (inc undo-arrow-width) undo-radius) (* 1.75 PI))
         (circle-pos (* (- 1 undo-arrow-width) undo-radius) (* 1.75 PI))
         (mapv +
               (circle-pos undo-radius (* 1.75 PI))
               (repeat (* undo-radius undo-arrow-length)))]]]
      ["replace"
       [[:polyline {:stroke highlight-color
                    :class "highlight"
                    :stroke-width 4
                    :stroke-linecap "round"}
         (list [40 50] [55 50])]
        [:polygon {:fill highlight-color
                   :class "highlight"}
         [52.5 40]
         [52.5 60]
         [62.5 50]]
        [:circle {:fill foreground-color
                  :class "foreground"}
         [20 50]
         15
         c/bubble-thickness]
        [:circle {:fill foreground-color
                  :class "foreground"}
         [80 50]
         15
         c/bubble-thickness]
        (text [20 50]
              20
              "x")
        (text [80 50]
              20
              "y")]]
      ["literal-fn-replaced"
       (vec
        (concat
         [[:polyline {:stroke highlight-color
                      :class "highlight"
                      :stroke-width 4
                      :stroke-linecap "round"}
           (list [40 50] [55 50])]
          [:polygon {:fill highlight-color
                     :class "highlight"}
           [52.5 40]
           [52.5 60]
           [62.5 50]]
          (outline-circle {:stroke foreground-color
                           :class "foreground"}
                          [20 50]
                          15
                          (- 1 c/bubble-thickness))
          [:circle {:fill foreground-color
                    :class "foreground"}
           [20 50]
           11
           c/bubble-thickness]
          (outline-circle {:stroke foreground-color
                           :class "foreground"}
                          [80 50]
                          15
                          (- 1 c/bubble-thickness))
          [:circle {:fill foreground-color
                    :class "foreground"}
           [80 50]
           11
           c/bubble-thickness]
          (text [20 50]
                17
                "x")
          (text [80 50]
                17
                "y")]
         (let [circle-center [80 50]
               radius 15
               width (* 2 radius c/bubble-thickness)]
           (mapcat (fn [side]
                     (mapcat (fn [direction]
                               [(vec
                                 (concat [:polyline
                                          {:stroke foreground-color
                                           :class "highlight"
                                           :stroke-width width}]
                                         (map (fn [pos]
                                                (mapv +
                                                      pos
                                                      circle-center
                                                      (mapv *
                                                            (reverse side)
                                                            (repeat (* direction radius c/set-line-offset)))))
                                              [(mapv (partial * 0.9)
                                                     side
                                                     (repeat radius))
                                               (mapv (partial * (inc c/set-line-length))
                                                     side
                                                     (repeat radius))])))])
                             [1 -1]))
                   [[-1 0]
                    [1 0]
                    [0 -1]
                    [0 1]]))))]])))

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