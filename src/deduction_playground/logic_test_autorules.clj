(ns deduction-playground.logic-test-autorules
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(defn create-arg
  [arg]
  (cond 
    (symbol? arg) arg
    (list? arg) (first arg)
    :else (throw (Exception. (str "Can't create an argument - " arg)))))

; TODO gleiche Argumente mit Nummern versehen
(defn create-args
  [given]
  (let [numbers (take (count given) (iterate inc 1))
        args (into [] (map #(symbol (if (not (symbol? %1))
                                      (str (create-arg %1) %2)
                                      %1)) given numbers))]
    args))
    ;(conj args 'q)))

(def keywords #{'truth 'contradiction})

     
; TEST
(defn m []
  (let [args '[and a]
        args2 '[b]]
    `(fn ~args 
       (fresh ~args2
              (== (first ~args) `(~'~(first args) ~~(second args) ~~(first args2)))))))
  
;(defn create-body
;  [given & conclusion]
;  (for [parameter given]
;    (if (not (symbol? parameter))
;      `(~'== ~(create-arg parameter) ~parameter))))

(defn c
        [given]
        (let [m (map #(list `list %) (rest given))
              op (list `quote (first given))
              op1 (list `list op)
              res (concat (list op1) m)
              res1 (conj res `concat)
              res2 (conj (list res1) `seq)]
          res2))

(defn b
     [arg form]
     `(== ~arg ~(c form)))

(defn create-term
  [t]
  (cond
    (symbol? t) t
    (list? t) (c t)
    :else (throw (Exception. "Can't create term"))))

(defn create-result
  [conc]
  `(== ~'q ~(create-term (first conc))))

(defn create-body
  [arg given]
  (if (symbol? given)
    ()
    `(== ~arg ~(create-term given))))

(defn create-fresh-args
  [given conc]
  (let [gvars (reduce #(concat %1 (if (symbol? %2)
                         [%2]
                         (rest %2))) [] given)
        cvars (reduce #(concat %1 (if (symbol? %2)
                                    [%2]
                                    (rest %2))) [] conc)
        vars (distinct (concat gvars cvars))]
    (into [] vars)))

(defn f
  [given conc]
  (let [args (create-args given)
        body (remove #(empty? %) (map #(create-body %1 %2) args given))
        res (list (create-result conc))
        f (concat body res)
        f2 (conj f (create-fresh-args given conc) `fresh)]
    `(fn ~(conj args 'q)
       ~f2)))

  
  
  
  

