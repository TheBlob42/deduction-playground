(ns deduction-playground.proof-new
  (:require [deduction-playground.auto-logic :as log]
            [deduction-playground.scope :as scope]))5

(def id (atom 0))
(defn new-id []
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

(defn rev-infer
  "Creates \"(infer [premises] formula)\" like depiction of a given (sub)proof"
  [proof]
  (let [premises (into [] (map #(:body %) (filter #(or (= (:rule %) :assumption)
                                                       (= (:rule %) :premise)) proof)))]
    `(~'infer ~premises ~(:body (last proof)))))

(defn line-to-id
  [proof line]
  (if (not (vector? line))
    (:id (nth (flatten proof) (dec line)))
    [(line-to-id proof (first line)) (line-to-id proof (last line))]))
  

(defn id-to-line
  [proof id]
  (loop [p (flatten proof)
         l 1]
    (if (= (:id (first p)) id)
      l
      (recur (rest p) (inc l)))))

(defn between [x y] [x y])

;(defn get-item
;  "Returns a line or subproof from the given proof"
;  [proof id]
;  (if (not (vector? id))
;    (nth (flatten proof) (dec (id-to-line proof id)))
;    (loop [p proof]
;      (let [item (first p)]
;        (cond
;          (nil? item) nil
;          (vector? item) (if (and (= (:id (first item)) (first id))
;                                  (= (:id (last item)) (last id)))
;                           item
;                           (recur (into [] (concat item (subvec p 1)))))
;          :else (recur (subvec p 1)))))))
(defn get-item
  [proof line]
  (if (not (vector? line))
    (nth (flatten proof) (dec line))
    (loop [p proof
           l 1]
      (cond 
        (empty? p) nil
        (= (first line) l) (first p)
        (vector? (first p)) (recur (into [] (concat (first p) (subvec p 1))) (inc l))
        :else (recur (subvec p 1) (inc l))))))

(defn check-args
  [proof rule lines forward?]
  (cond
    ; rule exists
    ; right number of arguments
    ; args distinct?
    ; all lines in range of proof?
    forward?
    (let [lastline (last (sort-by #(if (vector? %) (first %) %) lines))
          items (map #(get-item proof %) lines)
          scope (scope/scope-for-elem proof (get-item proof lastline))
;          arg-scope (take-while #(not (contains? (set todos) %)) scope)
          todos (scope/get-todo scope)
          results (scope/get-result scope)]
      (cond
        (not-every? #(contains? (set scope) %) items)
        (println "Not all in same scope")
        
        (empty? todos)
        (println "No open line to work towards")
        
        (some #(contains? (set items) %) todos)
        (println "Can't use empty line for forward resulting")
        
        (some #(contains? (set items) %) results)
        (println "Can't use result line for forward resulting")
        
;        (not-every? #(contains? (set arg-scope) %) items)
;        (println "Can't use items from before and from behind the todo-line")
        
        :else {:lastline lastline
               :items items
               :todos todos
               :results results}))
    
    (not forward?)
    (let [line (first lines)
          item (get-item proof line)
          scope (scope/scope-for-elem proof item)
          todos (scope/get-todo scope)
          results (rest (drop-while #(not= (first todos) %) (reverse scope)))]
      (cond 
        ; More than one line
        ; No todos
        ; More than one todo
        ; Used a line with rule != nil to start from
        ; Used a todo line to start from
        :else {:item item
               :todos todos
               :results results}))
    ))

(defn item-to-rule-arg
  [item]
  (if (not (vector? item))
    (:body item)
    (rev-infer item)))

(defn choose-option
  [proof line option]
  (let [item (get-item proof line)
        options (:body item)
        o (get options option)]
    (cond 
      (not (map? options))
      (throw (Exception. (str "There is nothing to choose in line " line)))
      
      (nil? o)
      (throw (Exception. (str "There is no optionsion " option " to choose")))
      
      :else
      (let [scope (scope/scope-for-line proof line)
            todos (scope/get-todo scope)
            results (scope/get-result scope)
            res (if (vector? o) o [o])
            match (filter #(= % (:body (first results))) res)
            rest (remove #(= % (:body (first results))) res)
            new-proof (if (not (empty? match))
                        (scope/remove-item (scope/change-item proof (first results) {:id (:id (first results))
                                                                                     :body (:body (first results))
                                                                                     :rule (:rule item)}) 
                                           (first todos))
                        proof)]
        (scope/remove-item (reduce #(scope/add-after-item %1 item {:id (new-id)
                                                                   :body %2
                                                                   :rule (:rule item)}) new-proof rest) item)))))


(defn step-f
  [proof rule & lines]
  (let [info   (check-args proof rule lines true)
        lastline (:lastline info)
        items  (:items info)
        todos  (:todos info)
        results (:results info)
        ids (map #(line-to-id proof %) lines)
        rule-args (map #(item-to-rule-arg %) items)
        rule-result (apply log/apply-rule1 (concat [rule true] rule-args))]
    (cond
      (empty? rule-result)
      (throw (Exception. "Incorrect parameters for the given rule"))
      
      (> (count rule-result) 1)
      (scope/add-after-line proof 
                            lastline 
                            {:id (new-id)
                             :body (apply merge (map-indexed #(hash-map (inc %1) %2) rule-result))
                             :rule (pr-str rule ids)}) 
      
      :else
      (let [res (if (vector? (first rule-result)) (first rule-result) rule-result)
            match (filter #(= % (:body (first results))) res)
            rest (remove #(= % (:body (first results))) res)
            new-proof (if (not (empty? match))
                        (scope/remove-item (scope/change-item proof (first results) {:id (:id (first results))
                                                                                     :body (:body (first results))
                                                                                     :rule (pr-str rule ids)}) 
                                           (first todos))
                        proof)]
        (reduce #(scope/add-after-line %1 lastline {:id (new-id)
                                                    :body %2
                                                    :rule (pr-str rule ids)}) new-proof rest)))))

(defn create-item
  [body] 
  (if (and (list? body)
           (contains? #{'infer} (first body)))
    (condp = (first body)
      'infer (eval (conj (map #(list `quote %) (rest body)) 'infer)))
    {:id (new-id)
     :body body
     :rule nil}))
      

(defn step-b
  [proof rule & lines]
  (let [info (check-args proof rule lines false)
        item (:item info)
        todos (:todos info)
        results (:results info)
        rule-args (item-to-rule-arg item)
        rule-result (apply log/apply-rule1 (concat [rule false] (list rule-args)))]
    (cond
      (empty? rule-result)
      (throw (Exception. "Incorrect parameters for the given rule"))
      
      :else
      (let [res (if (vector? (first rule-result)) (first rule-result) rule-result)
            bresults (set (map :body results))
            match (filter #(contains? bresults %) res)
            match-ids (map #(if (vector? %) [(:id (first %)) (:id (last %))] (:id %)) (filter #(contains? (set match) (:body %)) results))
            rest (remove #(contains? bresults %) res)
            rest-items (map create-item rest)
            rest-ids (map #(if (vector? %) [(:id (first %)) (:id (last %))] (:id %)) rest-items)]
        (cond-> (reduce #(scope/add-before-item %1 item %2) proof rest-items)
          (or (empty? rest)
              (every? vector? rest-items)) (scope/remove-item (first todos))
          true (scope/change-item item {:id (:id item)
                                        :body (:body item)
                                        :rule (pr-str rule (concat match-ids rest-ids))}))))))
                                        
          
;            new-proof (if (or (empty? rest)
;                              (every? vector? rest-items))
;                        (scope/remove-item proof (first todos))
;                        proof)
;            new-proof2 (reduce #(scope/add-before-item %1 item %2) new-proof rest-items)]
;        (scope/change-item new-proof2 item {:id (:id item)
;                                            :body (:body item)
;                                            :rule (pr-str rule (concat match-ids rest-ids))})))))

  
  
      
      
    
    