(ns deduction-playground.proof
  (:require [deduction-playground.parser-test :refer [wff?]]
            [deduction-playground.auto-logic :as log]
            [deduction-playground.scope :as scope]))

(def p '[{:id 1 :body a :rule :premise}
         {:id 2 :body b :rule :premise}
         {:id 3 :body :todo :rule nil}
         {:id 4 :body (and a b) :rule nil}])
(def p2 '[{:id 1 :body a :rule :premise}
          {:id 2 :body b :rule :premise}
          {:id 3 :body :todo :rule nil}
          {:id 4 :body (or a (and b c)) :rule nil}])
(def p3 '[{:id 1 :body (and e f) :rule :premise}
          {:id 2 :body (not f) :rule :premise}
          [{:id 3 :body f :rule :assumption}
           {:id 4 :body :todo :rule nil}
           {:id 5 :body e :rule nil}]
          {:id 6 :body e :rule nil}])
(def p4 '[{:id 1 :body (or a b) :rule :premise}
          [{:id 2 :body a :rule :assumption}
           {:id 3 :body :todo :rule nil}
           {:id 4 :body X :rule nil}]
          [{:id 5 :body b :rule :assumption}
           {:id 6 :body :todo :rule nil}
           {:id 7 :body X :rule nil}]
          {:id 8 :body :todo :rule nil}])
(def p5 '[{:bla 1}
          [{:bla 2}
           [{:bla 3}
            {:bla 4}
            {:bla 5}]
           [{:bla 6}
            {:bla 7}
            {:bla 8}]
           {:bla 9}]
          {:bla 10}])
(def p6 '[{:id 1 :body (exists [x] (P x)) :rule :premise}
          [{:id 2 :body (actual z) :rule :assumption}
           {:id 3 :body (substitution (P x) z x) :rule :assumption}
           {:id 4 :body :todo :rule nil}
           {:id 5 :body X :rule nil}]
          {:id 6 :body :todo :rule nil}
          {:id 7 :body X :rule nil}])

; TODO
; - "step" erweitern zusätzliche Prüfungen, mehrere Ergebnisse, Austauschen von neuen Parametern

(def id (atom 0))
(defn new-id
  []
  (swap! id inc))

(defn infer
  "Creates a proof from the given premise(s) (optional | use a vector for multiple ones) and a formula"
  ([formula] (infer [] formula))
  ([premises formula]
    (reset! id 0)
    (let [prem (if (vector? premises) 
                 (into [] (map #(hash-map :id (new-id)
                                          :body %
                                          :rule :premise) premises))
                 [{:id (new-id) :body premises :rule :premise}])
          todo {:id (new-id) :body :todo :rule nil}
          form {:id (new-id) :body formula :rule nil}]
      (conj prem todo form))))

(defn between [x y] (into [] (range x (inc y))))

(defn get-item-on-line
  "Returns a line or subproof from the given proof
x => returns line x
[x y z] => returns subproof between x and z (including all items and possible subsubproofs)"
  [proof line]
  (if (not (vector? line))
    (nth (flatten proof) (dec line))
    (loop [p proof
           l 1]
      (cond
        (empty? p) nil
        (= (first line) l) (first p)
        (vector? (first p)) (recur (into [] (concat (first p) (subvec p 1))) l)
        :else (recur (subvec p 1) (inc l))))))

(defn check-args 
  "Checks for erros inside the arguments. 
If no errors are found returns a map with :scope, :lastline, :todo and :result"
  [proof rule lines]
  (cond 
    (not (log/does-rule-exist? rule)) 
    (throw (Exception. (str "There is no rule called: \"" rule "\"")))
                 
    (not= (log/count-rule-given rule) (count lines)) 
    (throw (Exception. (str "Wrong number of arguments for rule \"" rule "\" (given: " (count lines) " | needed: " (log/count-rule-given rule) ")")))
                 
    (not (apply distinct? lines))
    (throw (Exception. "Duplicate lines"))
    
    (not-every? #(and (> % 0) 
                      (< % (inc (count (flatten proof))))) (flatten lines))
    (throw (Exception. (str "You can't refer to lines outside the proofs range (min: 1 | max: " (count (flatten proof)))))
    
    :else 
    (let [lastline (last (sort-by #(if (vector? %) (first %) %) lines))
          scope (scope/scope-for-line proof lastline)
          todo (scope/get-todo scope)
          result (scope/get-result scope)
          items (map #(get-item-on-line proof %) lines)]
      (cond
        (not-every? #(contains? (set scope) %) items)
        (throw (Exception. "Not all given lines are in the same scope"))
        
        (empty? todo)
        (throw (Exception. "There is no open line to work towards in the scope"))
        
        (or (> (count todo) 1)
            (> (count result) 1))
        (throw (Exception. "More than one todo/result step"))
        
        (contains? (set items) (first todo))
        (throw (Exception. "You can't use an empty line for proofing"))
        
        (contains? (set items) (first result))
        (throw (Exception. "You can't use a result line for forward proofing"))
        
        :else
        {:scope scope
         :lastline lastline
         :todo (first todo)
         :result (first result)}))))

(defn arg-for-line
  "Creates an argument from the given line for the usage with a deduction-rule.
In case of a single line, only the :body is returned.
In case of a subproof, this function will build a representation in the form => (infer [premises] formula)"
  [proof line]
  (let [item (get-item-on-line proof line)]
    (cond 
      (map? item) (:body item)
      (vector? item) 
      (let [premises (into [] (map #(:body %) (filter #(= (:rule %) :assumption) item)))]
        `(~'infer ~premises ~(:body (last item))))
      :else (throw (Exception. "Unknown type of element inside the given proof")))))

(defn step-f
  [proof rule & lines]
  (let [info (check-args proof rule lines)
        lastline (:lastline info) 
        todo-item (:todo info)
        result-item (:result info)
        args (map #(arg-for-line proof %) lines)
        result (apply log/apply-rule (concat [rule true] args))]
    (cond 
      (> (count result) 1)
      ;TODO Mehrere Möglichkeiten => Auswahlmöglichkeit
      (vector? (first result))
      ;TODO Mehr als ein Ergebniss => Mehrere Ergebnisse müssen eingepflegt werden
      :else ;Genau ein Ergebnis
      (if (= (first result) (:body result-item))
        (scope/remove-item (scope/change-item proof result-item {:id (:id result-item)
                                                                 :body (first result)
                                                                 :rule (str rule " " lines)}) todo-item)
        (scope/add-line proof lastline {:id (new-id)
                                        :body (first result)
                                        :rule (str rule " " lines)})))))


  
  