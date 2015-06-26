(ns deduction-playground.proof
  (:require [deduction-playground.parser-test :refer [wff?]]
            [deduction-playground.auto-logic :as log]))

(def p '[{:body a :rule :premise}
         {:body b :rule :premise}
         {:body :todo :rule nil}
         {:body (and a b) :rule nil}])
(def p2 '[{:body a :rule :premise}
          {:body b :rule :premise}
          {:body :todo :rule nil}
          {:body (or a (and b c)) :rule nil}])
(def p3 '[{:body (and e f) :rule :premise}
          {:body (not f) :rule :premise}
          [{:body f :rule :assumption}
           {:body :todo :rule nil}
           {:body e :rule nil}]
          {:body e :rule nil}])
(def p4 '[{:body (or a b) :rule :premise}
          [{:body a :rule :assumption}
           {:body :todo :rule nil}
           {:body X :rule nil}]
          [{:body b :rule :assumption}
           {:body :todo :rule nil}
           {:body X :rule nil}]
          {:body X :rule "or-e (1 (2-4) (5-7))"}])

(defn proof
  "Takes a proof in the form \"(infer <formula>)\" or \"(infer [premises] <formula>)\" and transforms it
into the inner representation to work with"
  [p]
  (if (and (list? p)
           (= (first p) 'infer))
    (cond
      (= (count (rest p)) 1) 
      [{:body :todo :rule nil}
       {:body (second p) :rule nil}]
      (and (= (count (rest p)) 2)
           (vector? (second p)))
      (let [premises (map #(hash-map :body %
                                     :rule :premise) (second p))]
        (into [] (concat premises [{:body :todo :rule nil}
                                   {:body (second (rest p)) :rule nil}])))
      :else (throw (Exception. "A valid proof needs excactly one or two arguments: (\"(infer <formula>)\" or \"(infer [premises] <formula>)\")")))
    (throw (Exception. "A valid proof has to be a list in the form: \"(infer <formula>)\" or \"(infer [premises] <formula>)\""))))

(defn todo
  "Returns the open line of the proof TODO: Should consider actual scope"
  [proof]
  (loop [p proof
         line 1]
    (if (= :todo (:body (first p)))
      line
      (recur (subvec p 1) (inc line)))))


(defn between
  [x y]
  (into [] (range x (inc y))))

(defn scope1
  "Creates the scopes representation for a given proof
[{...} {...} [{...} {...}] {...}] => [1 2 [3 4] 5]"
  ([proof] (scope1 proof 1))
  ([proof line] 
    (loop [p proof
           scope []
           l line]
      (if (empty? p)
        scope
        (if (vector? (first p))
          (recur (subvec p 1) (conj scope (scope1 (first p) l)) (+ (count (flatten (first p))) l))
          (recur (subvec p 1) (conj scope l) (inc l)))))))
        
(defn scope-for
  "Returns the scope for a specific line of a given proof
[1 2 [3 4] [5 6] 7] 6 => [1 2 [3 4] 5 6]"
  [scope line]
  (loop [ts scope
         s []]
    (cond
      (empty? ts) nil
      (= (first ts) line) (conj s line)
      (vector? (first ts))
      (if-let [underscope (scope-for (first ts) line)]
        (into [] (concat s underscope))
        (recur (subvec ts 1) (conj s (first ts))))
      :else 
      (recur (subvec ts 1) (conj s (first ts))))))

(defn inner-scope
  "Returns the inner scope of a specific line
[1 2 [3 4] [5 6 7] 8 9] 5 => [5 6 7]"
  [scope line]
  (if (contains? (set scope) line)
    scope
    (loop [ts scope]
      (cond 
        (empty? ts) nil
        (vector? (first ts)) 
        (if-let [s (inner-scope (first ts) line)]
          s
          (recur (subvec ts 1)))
        :else (recur (subvec ts 1))))))

(defn todo-in-scope
  [proof lines]
  (let [last-line (last (sort (flatten lines)))
        inner-scope (inner-scope (scope1 proof) last-line)
        todos (filter #(= (:body (nth (flatten proof) (dec %))) :todo) (remove vector? inner-scope))]
    (cond
      (> (count todos) 1) (throw (Exception. "More than one open proof step"))
      (< (count todos) 1) (throw (Exception. "No open proof step in choosen scope"))
      :else (first todos))))
      
(defn in-scope?
  "Checks if all lines of the given proof are in the same scope"
  [proof lines]
  (let [last-line (last (sort (flatten lines)))
        scope (scope-for (scope1 proof) last-line)]
    (every? #(contains? (set scope) %) lines)))


    
(defn check-step-args
  "Checks if all arguments for the proof step are valid"
  [proof rule lines]
  (cond
    (not (log/does-rule-exist? rule))
    (throw (Exception. (str "There is no rule named: " rule)))
    
    (not= (log/count-rule-given rule) (count lines))
    (throw (Exception. (str "Wrong number of arguments/lines provided for the rule \"" rule "\" (given: " (count lines) " | need: " (log/count-rule-given rule) ")")))
    
    (not (apply distinct? lines))
    (throw (Exception. (str "Duplicate line numbers " lines)))
    
    (not-every? #(and (> % 0) 
                      (< % (inc (count (flatten proof))))) lines)
    (throw (Exception. (str "You can't refer to lines outside the proofs range (min: 1 | max: " (count (flatten proof)))))
    
    (not (in-scope? proof lines))
    (throw (Exception. "The given lines are not all in the same scope"))))
                    

(defn step 
  [proof rule & lines]
  (do
    (check-step-args proof rule lines)
    (let [todo (todo-in-scope proof lines)
          args (map #(:body (nth proof (dec %))) lines)
          result (apply log/apply-rule (concat [rule] args))]
      (cond 
        (> (count result) 1)
        "TODO: Mehre Möglichkeiten - Auswahlmöglichkeit"
        
        (vector? (first result))
        "TODO: Mehrere Ergebnisse - Einpflegen in den Beweis"
        
        :else 
        ;TODO Bei der angewendeten Regel die verwendeten Zeilen "rule (1, 2)" anhängen
        (if (= (first result) (:body (last proof)))
          (conj (subvec proof 0 (dec (todo proof))) (assoc (last proof) :rule rule))
          (into [] (concat (subvec proof 0 (dec (todo proof))) [{:body (first result)
                                                                 :rule rule}] (subvec proof (dec (todo proof))))))
      ))))
  
  