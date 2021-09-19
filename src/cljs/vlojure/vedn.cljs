(ns vlojure.vedn
  (:require [vlojure.util :as u]
            [clojure.set :as sets]))

;;; This file defines an edn/clj(s) reader that creates "vedn" ("visual edn")
;;; objects. The most important functions in this file are "clj->vedn" and
;;; "vedn->clj", which can be used to convert clojure(script) code to vedn
;;; and vice versa, respectively.

(def whitespace-characters #{" " "\t" "\n" \,})

(def encapsulators ["'"
                    "@"
                    "`"
                    "~@"
                    "~"
                    "#_"
                    "^"
                    "#'"])

(def encapsulator-types [:quote
                         :deref
                         :syntax-quote
                         :unquote-splice
                         :unquote
                         :comment
                         :meta
                         :var-quote])

(def encapsulator->type (zipmap encapsulators
                                encapsulator-types))

(def opener->type {"(" :list
                   "[" :vector
                   "{" :map
                   "#{" :set
                   "#(" :lit-fn})

(def type->opener (sets/map-invert opener->type))

(def type->closer {:list ")"
                   :vector "]"
                   :map "}"
                   :set "}"
                   :lit-fn ")"})

(def closer-tokens (set (vals type->closer)))

(def special-tokens (vec (concat (keys opener->type)
                                 encapsulators
                                 (seq closer-tokens)
                                 ["##Inf"
                                  "##-Inf"
                                  "##NaN"
                                  "#inst"
                                  "#uuid"
                                  "#js"])))

(defn break-token [token]
  (reverse
   (filter seq
           (let [size (count token)]
             (loop [index 0
                    subtokens '()
                    subtoken-start 0
                    in-string? false]
               (if (>= index size)
                 (conj subtokens (subs token subtoken-start))
                 (let [special-token (some (fn [special-token]
                                             (when (= special-token
                                                      (subs token index (+ index (count special-token))))
                                               special-token))
                                           special-tokens)
                       quote? (and (= (nth token index) \")
                                   (not= (subs token (dec index) index) \\))]
                   (if in-string?
                     (if quote?
                       (recur (inc index)
                              (conj subtokens
                                    (subs token
                                          subtoken-start
                                          (inc index)))
                              (inc index)
                              false)
                       (recur (inc index)
                              subtokens
                              subtoken-start
                              true))
                     (if special-token
                       (let [special-token-size (count special-token)]
                         (recur (+ index special-token-size)
                                (conj subtokens
                                      (subs token
                                            subtoken-start
                                            index)
                                      special-token)
                                (+ index special-token-size)
                                false))
                       (if (and (= (nth token index) \#)
                                (= (first (subs token (inc index))) \"))
                         (recur (+ 2 index)
                                (conj subtokens
                                      (subs token
                                            subtoken-start
                                            index))
                                index
                                true)
                         (if quote?
                           (recur (inc index)
                                  (conj subtokens
                                        (subs token
                                              subtoken-start
                                              index))
                                  index
                                  true)
                           (recur (inc index)
                                  subtokens
                                  subtoken-start
                                  false))))))))))))

(defn tokenize-clj [clj-str]
  (let [size (count clj-str)]
    (loop [str-index 0
           past-tokens '()
           token-start nil
           in-str? false]
      (if (>= str-index size)
        (vec
         (if token-start
           (concat past-tokens
                   (break-token (subs clj-str token-start)))
           past-tokens))
        (let [current-character (nth clj-str str-index)
              whitespace? (whitespace-characters current-character)
              quote? (= current-character \")]
          (if in-str?
            (if quote?
              (if (= (nth clj-str (dec str-index)) \\)
                (recur (inc str-index)
                       past-tokens
                       token-start
                       true)
                (recur (inc str-index)
                       (if token-start
                         (concat past-tokens
                                 (break-token (subs clj-str token-start (inc str-index))))
                         past-tokens)
                       nil
                       false))
              (recur (inc str-index)
                     past-tokens
                     token-start
                     true))
            (if (and (= current-character \#)
                     (= (first (subs clj-str (inc str-index))) \"))
              (recur (+ 2 str-index)
                     past-tokens
                     (or token-start str-index)
                     true)
              (if whitespace?
                (recur (inc str-index)
                       (if token-start
                         (concat past-tokens
                                 (break-token (subs clj-str token-start str-index)))
                         past-tokens)
                       nil
                       false)
                (recur (inc str-index)
                       past-tokens
                       (or token-start str-index)
                       quote?)))))))))

(defn strip-single-line-comments [clj-str]
  (loop [index 0
         stripped-chars []
         comment? false
         in-string? false]
    (if (>= index (count clj-str))
      (apply str stripped-chars)
      (let [character (nth clj-str index)
            quote? (and (not comment?)
                        (= character \")
                        (or (zero? index)
                            (not (= (nth clj-str (dec index)) \\))))]
        (if in-string?
          (recur (inc index)
                 (conj stripped-chars character)
                 false
                 (not quote?))
          (let [in-comment? (or comment? (= character \;))]
            (recur (inc index)
                   (if in-comment?
                     stripped-chars
                     (conj stripped-chars character))
                   (and in-comment?
                        (not= character \newline))
                   quote?)))))))

(defn literal-form [token]
  {:type :literal
   :value token})

(defn get-child [form path]
  (if (empty? path)
    form
    (get-child (nth (:children form)
                    (first path))
               (rest path))))

(defn add-child [form path child]
  (if (empty? path)
    (update form
            :children #(conj % child))
    (update-in form
               [:children (first path)]
               #(add-child %
                           (rest path)
                           child))))

(defn insert-child [form path child]
  (if (= (count path) 1)
    (update form
            :children #(u/vector-insert % (inc (first path)) child))
    (update-in form
               [:children (first path)]
               #(insert-child %
                              (rest path)
                              child))))

(defn replace-child [form path child]
  (if (empty? path)
    child
    (update-in form
               [:children (first path)]
               #(replace-child %
                               (rest path)
                               child))))

(defn remove-child [form path]
  (if (= (count path) 1)
    (update form
            :children
            #(u/vector-remove % (first path)))
    (update-in form
               [:children (first path)]
               #(remove-child % (rest path)))))

(defn tokens->vedn [tokens]
  (loop [token-index 0
         form {:type :vector :children []}
         path []
         encapsulator-depths []]
    (if (>= token-index (count tokens))
      form
      (let [token (nth tokens token-index)
            encapsulator-type (encapsulator->type token)
            opener-type (opener->type token)]
        (if encapsulator-type
          (recur (inc token-index)
                 (add-child form path
                            {:type encapsulator-type
                             :children []})
                 (conj path (count (:children (get-child form path))))
                 (conj encapsulator-depths (inc (count path))))
          (if opener-type
            (recur (inc token-index)
                   (add-child form path
                              {:type opener-type
                               :children []})
                   (conj path (count (:children (get-child form path))))
                   encapsulator-depths)
            (if (closer-tokens token)
              (if (and (seq encapsulator-depths)
                       (= (count path)
                          (inc (last encapsulator-depths))))
                (recur token-index
                       form
                       (pop path)
                       (pop encapsulator-depths))
                (recur (inc token-index)
                       form
                       (pop path)
                       encapsulator-depths))
              (if (= (count path)
                     (last encapsulator-depths))
                (recur (inc token-index)
                       (add-child form path (literal-form token))
                       (pop path)
                       (pop encapsulator-depths))
                (recur (inc token-index)
                       (add-child form path (literal-form token))
                       path
                       encapsulator-depths)))))))))

(defn clj->vedn [clj-str]
  (-> clj-str
      strip-single-line-comments
      tokenize-clj
      tokens->vedn))

(defn vedn->clj [form]
  (let [{:keys [type value children]} form]
    (if (= type :literal)
      value
      (str (type->opener type)
           (apply str
                  (map (fn [child first?]
                         (str (when (not first?) " ")
                              (vedn->clj child)))
                       children
                       (conj (repeat false) true)))
           (type->closer type)))))