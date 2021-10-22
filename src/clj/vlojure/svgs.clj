(ns vlojure.svgs
  (:require [clojure.java.io :as java-io]
            [dali.io :as io]))

(defn rgb
  "Takes an integer `c` between 0 and 0xffffff, inclusive, and returns a string
   representing the same color formatted for use in svgs."
  [c]
  (str "#" (format "%06x" c)))


; Constants used to define SVGs
(def size 100)
(def white (rgb 0xffffff))
(def output-directory "./generated_svgs/")

#_("undo" "redo" "replace" "literal-fn-replace" "enclose" "vector-enclose" "fn-enclose" "let-enclose" "comment")

; "documents" defines all SVGs that will be exported when "export" is run
(def documents
  (mapv
   (fn [[name & doc-fragments]]
     [name
      (vec
       (conj doc-fragments
             [:rect {:fill (rgb 0)}
              [0 0]
              [size size]]
             {:width size :height size}
             :dali/page))])
   [["undo"
     [:polygon {:fill white}
      [0 0]
      [50 0]
      [0 50]]]]))

(defn render-document
  "Takes in a dali `document` and a `filename`, and saves the document as an
   SVG with the given file name"
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