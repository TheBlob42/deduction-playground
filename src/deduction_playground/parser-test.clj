(ns deduction-playground.parser-test
  (:require [clojure.string :as s]))

; just for testing
(def sign {:c [:const 0]
           :f [:func 2]
           :f3 [:func 1]
           :P [:pred 1]
           :P2 [:pred 2]
           :f2 [:func 0]})


(defn is? 
  "Verifies if symb (has to be a symbol) is part of the signature and holds the given key"
  [symb key]
  (if (= (first ((keyword symb) sign)) key)
    true
    false))

(defn variable?
  "Verifies if symb (has to be a symbol) is not part of the signature"
  [symb]
  (if (not ((keyword symb) sign))
    true
    false))

(defn check-arguments
  "Verifies the arguments of phi by checking the arity in the signature and validate each argument with the function f"
  [phi f]
  (let [arity (second ((keyword (first phi)) sign))]
    (if (= (count (rest phi)) arity)
      (every? f (rest phi))
      (throw (Exception. (str "Wrong number of arguments for the element \"" (first phi) "\" (signature: " arity ") - " phi))))))
      
(defn atom
  "Verifies if phi is a constant or a variable"
  [phi]
  (and (symbol? phi)
       (or (is? phi :const)
           (variable? phi))))

(declare function)
(defn term
  "Verifies if phi is a valid term\nThe parent is just for better error messages"
  [phi & [parent]]
  (if (or (atom phi)
          (function phi))
    true
    (throw (Exception. (str phi " is not a valid term" (if parent (str " - " parent) ""))))))

(defn function
  "Verifies if phi is a valid function"
  [phi]
  (and (list? phi)
       (is? (first phi) :func)
       (check-arguments phi #(term % phi))))

(defn predicate
  "Verifies if phi is a valid predicate"
  [phi]
  (and (is? (first phi) :pred)
       (check-arguments phi #(term % phi))))
  
(defn subform
  "Verifies if phi is a valid formula"
  [phi]
  (cond
    (= phi true)  true
    (= phi false) true
    (not (list? phi)) (throw (Exception. (str "\"" phi "\" is not a valid subformula")))

    (predicate phi) true
    
    (= (first phi) '=)
    (if (not= (count (rest phi)) 2)
      (throw (Exception. (str "\"=\" must have exactly two arguments (currently " (count (rest phi)) ") - " phi)))
      (every? term (rest phi)))
    
    (= (first phi) 'and)
    (if (< (count (rest phi)) 2)
      (throw (Exception. (str "\"and\" must have at least two arguments (currently " (count (rest phi)) ") - " phi)))
      (every? subform (rest phi)))
    
    (= (first phi) 'or)
    (if (< (count (rest phi)) 2)
      (throw (Exception. (str "\"or\" must have at least two arguments (currently " (count (rest phi)) ") - " phi)))
      (every? subform (rest phi)))
    
    (= (first phi) 'not)
    (if (> (count (rest phi)) 1)
      (throw (Exception. (str "\"not\" must not have more than one argument (currently " (count (rest phi)) ") - " phi)))
      (subform (second phi)))
    
    (= (first phi) 'forall)
    (if (not= (count (rest phi)) 2)
      (throw (Exception. (str "\"forall\" must have exactly two arguments (currently " (count (rest phi)) ") - " phi)))
      (if-not (and (vector? (second phi))
                   (> (count (second phi)) 0))
        (throw (Exception. (str "The second argument of the \"forall\" operator has to be a vector with at least one variable definition - " phi)))
        (if-not (every? variable? (second phi))
          (throw (Exception. (str "There are non-variable elements inside the variable definition - " phi)))
          (subform (second (rest phi))))))
    
    (= (first phi) 'exist)
    (if (not= (count (rest phi)) 2)
      (throw (Exception. (str "\"exist\" must have exactly two arguments (currently " (count (rest phi)) ") - " phi)))
      (if-not (and (vector? (second phi))
                   (> (count (second phi)) 0))
        (throw (Exception. (str "The second argument of the \"exist\" operator has to be a vector with at least one variable definition - " phi)))
        (if-not (every? variable? (second phi))
          (throw (Exception. (str "There are non-variable elements inside the variable definition - " phi)))
          (subform (second (rest phi))))))
    
    :else (throw (Exception. (str "\"" (first phi) "\" is not a valid subformula operator (only: and, or, not, =, forall, exist) or a valid predicate")))))

(defn wff?
  [phi]
  (cond
    (list? phi) (subform phi)  ;TODO
    (symbol? phi) true;TODO
    :else false));TODO











