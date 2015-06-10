(ns deduction-playground.parser
  (:require [clojure.string :as s]))

(def signature (atom {}))
(def variables (atom {}))

(defn add-variable!
  "Adds a new var with the given key to the variables-atom. Returns false
if the var is already part of the atom but with another key. Otherwise the
var is added and the function returns true."
  [var key]
  (if-let [k ((keyword var) @variables)]
    (if (= k key)
      true
      false)
    (do
      (swap! variables assoc (keyword var) key)
      true)))
  
(def keywords #{"(" ")" "[" "]" "and" "or" "not" "forall" "exist" "="})

(defn bound-variable!
  "Generates a parsing function, that adds the first element of in to the
variables-atom as a bound variable. If it fails, the parsing fails."
  []
  (fn [in]
    (if (or (empty? in)
            ((keyword (first in)) @signature)
            (contains? keywords (first in)))
      [nil in]
      (if (add-variable! (first in) :bound)
        [[(first in)] (rest in)]
        [nil in]))))

(defn variable!
  "Generates a parsing function, that checks if the first element of in is a
variable or if possible adds it as a new free variable to the variable-atom"
  []
  (fn [in]
    (if (or (empty? in)
            ((keyword (first in)) @signature)
            (contains? keywords (first in)))
      [nil in]
      (do
        (if (not ((keyword (first in)) @variables))
          (add-variable! (first in) :free))
        [[(first in)] (rest in)]))))

  
; *** parsing functions ***
; see "Hertzberg - Chapter 8" for additional informations

(defn por
  "Generates a parsing function, that succeeds once the first parsers succeeds."
  [& parsers]
  (fn [in]
    (let [result (filter #(first %) (map #(% in) parsers))]
      (if (empty? result)
        [nil in]
        (first result)))))

(defn pipe
  "Helper function for the pand, pand-lazy and is-with-arity parsing functions."
  [parsers in]
  (lazy-seq
    (if (empty? parsers)
      ()
      (let [[res1 in1 :as result] ((first parsers) in)]
        (cons result (pipe (rest parsers) in1))))))

(defn pand 
  "Generates a parsing function, that succeeds if ALL of the parsers succeed."
  [& parsers]
  (fn [in] 
    (let [result (pipe parsers in)]
      (if (every? #(first %) result)
        [(into [] (reduce concat (map first result))) (second (last result))]
        [nil in]))))

(defn pand-lazy 
  "Generates a parsing function, that succeeds if ALL of the parsers succeed.
As distinct from pand this function is lazy. So it works until the first parser 
doesn't succeed and returns these values (which is better for understanding errors).\n
[[values that succeeded] (rest)] instead of [nil input]"
  [& parsers]
  (fn [in]
    (let [result (take-while #(first %) (pipe parsers in))]
      (if (empty? result)
        [nil in]
        [(into [] (reduce concat (map first result))) (second (last result))]))))

(defn more
  "Generating a parsing function, that succeeds if the given parser succeeds one
or more times, returning that sequence lazy."
  [parser]
  (apply pand-lazy (cycle [parser])))

(defn success [in] [[] in])
(defn fail [in] [nil in])
(defn optional [parser] (por parser success))
(defn many [parser] (optional (more parser)))
(defn exact [parser n] (apply pand-lazy (take n (cycle [parser]))))

(defn item 
  "Generates a parsing function, that checks whether the first element of in
is equal to the given itm."
  [itm]
  (fn [in]
    (if (empty? in)
      [nil in]
      (if (= (first in) itm)
        [[itm] (rest in)]
        [nil in]))))

(defn is 
  "Generates a parsing function, that checks whether the first element of in
is part of the signature-atom and holds the given key."
  [key]
  (fn [in]
    (if (empty? in)
      [nil in]
      (if (= (first ((keyword (first in)) @signature)) key)
        [[(first in)] (rest in)]
        [nil in]))))

(defn check
  "Generates a parsing function, that checks whether the first element of in
is part of the signature-atom and holds the given key (like is). Unlike is this 
function does NOT consume the element but instead returning the unchanged input"
  [key]
  (fn [in]
    (if (empty? in)
      [nil in]
      (if (= (first ((keyword (first in)) @signature)) key)
        [[] in]
        [nil in]))))

(defn is-with-arity
  "Generates a parsing function, that checks whether the first element of in
is part of the signature-atom and holds the given key. Also considers the arity
by checking that there exactly as much elements which satisfy the parser"
  [key parser]
  (fn [in]
    (if (empty? in)
      [nil in]
      (if (= (first ((keyword (first in)) @signature)) key)
        (let [arity (second ((keyword (first in)) @signature))
              parsers (take arity (cycle [parser]))
              result (take-while #(first %) (pipe parsers (rest in)))]
          (if (empty? result)
            [nil in]
            [(into [] (reduce concat [(first in)] (map first result))) (second (last result))])))))) 
    

; *** GRAMMAR ***

(declare term-parser)
(defn func-parser [in]
  ((pand-lazy (pand (item "(") (check :func)) (is-with-arity :func term-parser) (item ")")) in))

(defn term-parser [in]
  ((por 
     (is :const)
     (variable!)
     func-parser) in))

(def pred-parser
  (pand-lazy (pand (item "(") (check :pred)) (is-with-arity :pred term-parser) (item ")")))

(def equals-parser
  (pand-lazy (pand (item "(") (item "=")) term-parser term-parser (item ")")))

(declare form-parser)
(defn form-parser [in]
  ((por
    (por (item "true") (pand (item "(") (item "true") (item ")")))
    (por (item "false") (pand (item "(") (item "false") (item ")")))
    pred-parser
    equals-parser
    (pand-lazy (pand (item "(") (item "and")) form-parser form-parser (item ")"))
    (pand-lazy (pand (item "(") (item "or")) form-parser form-parser (item ")"))
    (pand-lazy (pand (item "(") (item "not")) form-parser (item ")")) 
    (pand-lazy (pand (item "(") (item "forall")) (item "[") (more (bound-variable!)) (item "]") form-parser (item ")"))
    (pand-lazy (pand (item "(") (item "exist")) (item "[") (more (bound-variable!)) (item "]") form-parser (item ")"))) in))

(defn parse-formula [sign form]
  (reset! signature sign)
  (reset! variables {})
  (if (empty? (second (form-parser form)))
    {:signature sign
     :variables @variables}
    nil))

; TODO
; - Sollen and & or mehr als zwei Formeln "aufnehmen" können z.B. (and form form form) (Lösung über many/more)
; - Sollen Predikate und Funktionen vllt. anders dargestellt werden z.B. P(x) und f(c x) statt (P x) (f c x) (Lösung Parser umschreiben)

; DEBUG
(defn split-form [form]
  (s/split (s/replace (str form) #"\(|\)|\[|\]" {"(" "( " ")" " )" "[" "[ " "]" " ]"}) #" "))

(def s {:c [:const 0]
        :P [:pred 1]
        :f [:func 2]})
    

