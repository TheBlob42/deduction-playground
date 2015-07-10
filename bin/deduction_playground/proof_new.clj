(ns deduction-playground.proof-new
  (:require [deduction-playground.auto-logic :as log]
            [deduction-playground.scope :as scope]))5

(def id (atom 0))
(defn new-id []
  (swap! id inc))

(defn infer
  "Creates a proof of the given premises (optional, vector for multiples) and the formula"
  ([formula] (infer [] formula))
  ([premises formula & [superproof?]]
    (let [desc (if superproof? :premise :assumption)
          prem (if (vector? premises) 
                 (into [] (map #(hash-map :id (new-id)
                                          :body %
                                          :rule desc) premises))
                 [{:id (new-id) :body premises :rule desc}])
          todo {:id (new-id) :body :todo :rule nil}
          form {:id (new-id) :body formula :rule nil}]
      (conj prem todo form))))

(defn rev-infer
  "Creates \"(infer [premises] formula)\" like depiction of the given (sub)proof"
  [proof]
  (let [premises (into [] (map #(:body %) (filter #(or (= (:rule %) :assumption)
                                                       (= (:rule %) :premise)) proof)))]
    `(~'infer ~premises ~(:body (last proof)))))

(defn proof
  "Starts a new superproof"
  ([formula] (proof [] formula))
  ([premises formula]
    (reset! id 0)
    (apply infer [premises formula true])))

(defn line-to-id
  [proof line]
  (if (not (vector? line))
    (:id (nth (flatten proof) (dec line)))
    [(line-to-id proof (first line)) (line-to-id proof (last line))]))

(defn id-to-line
  [proof id]
	(if (not (vector? id))
	  (loop [p (flatten proof)
	         l 1]
	    (if (= (:id (first p)) id)
	      l
	      (recur (rest p) (inc l))))
	  [(id-to-line proof (first id)) (id-to-line proof (last id))]))

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
    (let [lastline    (last (sort-by #(if (vector? %) (first %) %) lines))
          items       (map #(get-item proof %) lines)
          scope-info  (scope/get-scope proof (get-item proof lastline))
          scope       (:scope scope-info)
          todos       (:todo scope-info)
          conclusions (:conclusions scope-info)]
      (cond
        (not-every? #(contains? (set scope) %) items)
        (println "Not all in same scope")

        (> (count todos) 1)
        (println "There can't be more than one :todo line")
        
        (some #(contains? (set items) %) todos)
        (println "Can't use empty line for forward resulting")
        
        (some #(contains? (set items) %) conclusions)
        (println "Can't use conclusion line for forward resulting")
            
        :else {:lastline lastline
               :items items
               :todo (first todos)
               :conclusions conclusions}))
    
    (not forward?)
    (let [line     (first lines)
          item     (get-item proof line)
          scope-info (scope/get-scope proof item)
          scope    (:scope scope-info)
          todos    (:todo scope-info)
          premises (:premises scope-info)]
      (cond 
        ; More than one line
        ; No todos
        ; More than one todo
        ; Used a line with rule != nil to start from
        ; Used a todo line to start from
        :else {:item item
               :todo (first todos)
               :premises premises}))))

(defn item-to-rule-arg
  [item]
  (if (not (vector? item))
    (:body item)
    (rev-infer item)))

(defn create-item
  ([body] (create-item body nil))
  ([body rule]
	  (if (and (coll? body)
	           (contains? #{'infer} (first body)))
     (condp = (first body)
	      'infer (eval (conj (map #(list `quote %) (rest body)) 'infer)))
	    {:id (new-id)
	     :body body
	     :rule rule})))

; TODO not only check the conclusions but the premises as well
(defn choose-option
  [proof line option]
  (let [item (get-item proof line)
        options (:body item)
        opt (get options option)]
    (cond 
      (not (map? options))
      (throw (Exception. (str "There is nothing to choose in line " line)))
      
      (nil? opt)
      (throw (Exception. (str "There is no option \"" option "\" to choose")))
      
      :else
      (let [items (if (vector? opt) opt [opt])
            scope-info  (scope/get-scope proof item)
            todo-item   (first (:todo scope-info))
            conc-items  (:conclusions scope-info)
            conclusions (set (map :body conc-items))
            match-items (filter #(contains? (set items) (:body %)) conc-items)
            rest        (remove #(contains? conclusions %) items)
            rest-items  (map #(create-item % (:rule item)) rest)
            p1 (reduce #(scope/change-item %1 %2 {:id   (:id %2)
                                                  :body (:body %2)
                                                  :rule (:rule item)}) proof match-items)
            p2 (reduce #(scope/add-after-item %1 item %2) p1 rest-items)
            p3 (scope/remove-item p2 item)]
        (if (= (count conc-items) (count match-items))
          (scope/remove-item p3 todo-item) p3)))))

(defn step-f
  [proof rule & lines]
  (let [info         (check-args proof rule lines true)
        lastline     (:lastline info)
        line-items   (:items info)
        todo-item    (:todo info)
        conc-items   (:conclusions info)
        ids (map #(line-to-id proof %) lines)
        rule-args (map #(item-to-rule-arg %) line-items)
        rule-result (apply log/apply-rule1 (concat [rule true] rule-args))]
    (cond
      (empty? rule-result)
      (throw (Exception. (str "Incorrect parameters for the rule \"" rule "\". Please check the description.")))
      
      ; user has to choose which is the right permutation
      (> (count rule-result) 1)
      (scope/add-after-line proof 
                            lastline 
                            {:id   (new-id)
                             :body (apply merge (map-indexed #(hash-map (inc %1) %2) rule-result))
                             :rule (pr-str rule ids)}) 
      
      :else
      (let [result (if (vector? (first rule-result)) (first rule-result) rule-result)
            conclusions (set (map :body conc-items))
            matches     (filter #(contains? conclusions %) result)
            rest        (remove #(contains? conclusions %) result)
            match-items (filter #(contains? (set matches) (:body %)) conc-items)
            rest-items  (map #(create-item % (pr-str rule ids)) rest)
            p1 (reduce #(scope/change-item %1 %2 {:id   (:id %2)
                                                  :body (:body %2)
                                                  :rule (pr-str rule ids)}) proof match-items)
            p2 (reduce #(scope/add-after-line %1 lastline %2) p1 rest-items)]
        (if (= (count conc-items) (count match-items))
          (scope/remove-item p2 todo-item) p2)))))
      
; TODO backward step with more than one line
(defn step-b
  [proof rule & lines]
  (let [info (check-args proof rule lines false)
        line-item (:item info)
        todo-item (:todo info)
        prem-items (:premises info)
        rule-args (item-to-rule-arg line-item)
        rule-result (apply log/apply-rule1 (concat [rule false] (list rule-args)))]
    (cond
      (empty? rule-result)
      (throw (Exception. "Incorrect parameters for the given rule"))
      
      :else
      (let [result (if (vector? (first rule-result)) (first rule-result) rule-result)
            premises  (set (map :body prem-items))
            matches   (filter #(contains? premises %) result)
            rest      (into [] (remove #(contains? premises %) result))
            match-items (filter #(contains? (set matches) (:body %)) prem-items)
            match-ids   (map #(if (vector? %) [(:id (first %)) (:id (last %))] (:id %)) match-items)
            rest-items  (map create-item rest)
            rest-ids    (map #(if (vector? %) [(:id (first %)) (:id (last %))] (:id %)) rest-items)
            p1 (reduce #(scope/add-before-item %1 line-item %2) proof rest-items)
            p2 (scope/change-item p1 line-item {:id   (:id line-item)
                                                :body (:body line-item)
                                                :rule (pr-str rule (concat match-ids rest-ids))})]
        (if (or (empty? rest) (every? vector? rest-items))
          (scope/remove-item p2 todo-item) p2)))))
 
  
      
      
    
    