(ns deduction-playground.proof-new
  (:require [deduction-playground.proof :refer [get-item
                                                get-scope
                                                add-after-item 
                                                add-before-item 
                                                remove-item 
                                                replace-item]]
            [deduction-playground.auto-logic :as log]))

;; atoms to provide unique ids for items and variables
(def id (atom 0))
(defn new-id []
  (swap! id inc))

(def var-id (atom 0))
(defn new-var []
  (symbol (str 'V (swap! var-id inc))))
;; ---------------------------------------------------

;; functions for removing duplicate entries from the proof
(defn remove-duplicates
  "Checks proof for duplicate lines that are in the same scope and deletes them if possible.
The ids of the deleted lines and their replacement will be saved as meta-data in the form {deleted-id replacement-id [...]}"
  ([proof] (remove-duplicates proof proof))
  ([proof sub]
    (let [parent-meta (meta proof); why gets the meta-data lost when we move this definition some lines down???
          scope (get-scope proof (last sub))
          duplicates (disj (set (map first (filter #(> (val %) 1) (frequencies (map :body (remove vector? scope)))))) :todo)
          duplicate-items (filter #(contains? duplicates (:body %)) scope)
          equals (map val (group-by :body duplicate-items))
          fn-smap (fn [equals]
                    (let [remain (map :id (filter :rule equals))
                          delete (map :id (filter (set sub) (remove :rule equals)))]; just items from the actual sub can be deleted
                      (reduce #(assoc %1 %2 (last remain)) {} delete)))
          meta (apply merge (map fn-smap equals))
          delete-items (remove :rule (filter (set duplicate-items) sub))
          new-proof (with-meta (reduce #(remove-item %1 %2) proof delete-items) (merge meta parent-meta))]
;      (println (meta proof)); why is the meta-data lost down here???
      (reduce #(if (vector? %2) (remove-duplicates %1 %2) %1) new-proof sub))))

(defn adjust-ids
  "Adjusts the ids used within the :rule of the items.
Replaces deleted ids with their new target.
Information is provided by the meta-data created through \"remove-duplicates\"."
  [proof]
  (let [meta  (meta proof)
        ;; \b marks the word boundary to difference 10 from 1|0 etc.
        regex (java.util.regex.Pattern/compile (clojure.string/join "|" (map #(str "\\b" % "\\b") (map key meta))))
        smap  (apply merge (map #(hash-map (str (key %)) 
                                           (if (list? (val %))
                                             (clojure.string/join " " (val %));; swaps one id with a list of others
                                             (str (val %)))) meta))]
    (if (not-empty meta)
      (clojure.walk/postwalk 
        (fn [node]
          (if (vector? node)
            node
            (if (string? (:rule node))
              (assoc node :rule (clojure.string/replace (:rule node) regex #(get smap %)))
              node))) 
        proof)
      proof)))
    
(defn remove-todos
  "Removes all \"...\" lines, if all lines inside the (sub)proof are solved (rule =! nil)"
  [proof]
  (let [solved (< (count (remove :rule (remove #(= (:body %) :todo) (remove vector? proof)))) 1)]
    (loop [p proof
           np []]
      (cond 
        (empty? p) np
        (vector? (first p)) (recur (subvec p 1) (conj np (remove-todos (first p))))
        :else 
        (if (and solved (= (:body (first p)) :todo))
          (recur (subvec p 1) np)
          (recur (subvec p 1) (conj np (first p))))))))
          
(defn check-duplicates
  "Removes duplicate lines, adjust leftover ids and remove \"...\" lines if possible"
  [proof]
  (remove-todos (adjust-ids (remove-duplicates proof))))
;; -------------------------------------------------------

;; functions for special forms (e.g. infer, substitution)
(defn substitution
  "Substitutes an variable identifier inside a predicate formula with another
e.g. (substitution '(P x) 'x 'Z) => (P Z)"
  [formula old new]
  (cond
    (not (list? formula))
    (throw (Exception. (str "The argument \"" formula "\" is not a list and therefore can't be substituted." 
                            "Maybe you have to provide optional arguments for the step you trying to accomplish.")))
    
    (contains? (set (flatten formula)) new)
    (throw (Exception. (str "Substitution failed. The identifier \"" new "\" is already used inside the formula \"" formula "\"")))
    
    :else (clojure.walk/postwalk-replace {old new} formula)))

(defn between [x y] [x y])

(defn infer
  "Creates a (sub)proof for premises and formula in the structure needed for further actions.
\"premises\" can be a single object, a vector of objects or skipped
\"formula\" needs to be a single object
\"superproof?\" decides if its a proof or subproof" 
  ([formula] (infer [] formula))
  ([premises formula & [superproof?]]
    (let [desc (if superproof? :premise :assumption)
          prem (if (vector? premises) 
                 (into [] (map #(hash-map :id (new-id)
                                          :body %
                                          :rule desc) premises))
                 [{:id (new-id) :body premises :rule desc}])
          todo {:id (new-id) :body :todo :rule nil}
          ;; TEST with multiple conclusions
          forms (if (vector? formula)
                  (into [] (map #(hash-map :id (new-id)
                                           :body %
                                           :rule nil) formula))
                  [{:id (new-id) :body formula :rule nil}])]
      (check-duplicates (apply conj (conj prem todo) forms))))) 
;          form {:id (new-id) :body formula :rule nil}]
;      (check-duplicates (conj prem todo form)))))

(defn re-infer
  "Returns a (sub)proof from the internal strucure back to a depiction like \"(infer [premises] formula)\""
  [proof]
  (let [premises (into [] (map #(:body %) (filter #(or (= (:rule %) :assumption)
                                                       (= (:rule %) :premise)) proof)))
        prem-args (if (> (count premises) 1) premises (first premises))]
    `(~'infer ~prem-args ~(:body (last proof)))))

(defn proof
  "Creates a new superproof
This is the entry point for new deductions"
  ([formula] (proof [] formula))
  ([premises formula]
    (reset! id 0)
    (reset! var-id 0)
    (apply infer [premises formula true])))
;; ------------------------------------------------------

;; helping and utility functions
(defn get-proofed-items
  "Only returns items that are either vectors or have a rule (they are prooved)"
  [items]
  (filter #(or (vector? %)
               (:rule %)) items))

(defn get-unproofed-items
  "Only returns items that are neither vectors nor have a rule (they are unprooved so far)"
  [items]
  (remove #(or (vector? %)
               (:rule %)) items))
    
(defn item-to-rule-arg
  "Converts a item to a rule argument for auto-logic"
  [item]
  (if (not (vector? item))
    (:body item)
    (re-infer item)))

(defn get-item-id
  "Returns the id for the item. 
Item can also be a subproof (vector)"
  [item]
  (if (not (vector? item))
    (:id item)
    [(:id (first item)) (:id (last item))]))
;; -----------------------------
                    
;; functions for creating new proof items from given bodies
(defn init-vars
  "Replaces the automatically created variables from core.logic (\"_0\", \"_1\" etc.)
with new unique identifiers (\"V1\", \"V2\" etc.)"
  [bodies]
  (let [vars (set (filter #(.startsWith (str %) "_") (flatten bodies)))
        smap (reduce #(assoc %1 %2 (new-var)) {} vars)
        new-bodies (map #(if (symbol? %)
                           (if (contains? vars %) (get smap %) %)
                           (clojure.walk/prewalk-replace smap %)) bodies)]
    new-bodies))

(defn eval-body
  "Searches for keywords (infer, substitution) inside body and evaluates them. Otherwise returns body"
  [body]
  (if (and (list? body)
           (contains? #{'infer 'substitution} (first body)))
    (condp = (first body)
      'infer      (let [prem (if (vector? (second body))
                               (into [] (map #(eval-body %) (second body)))
                               (eval-body (second body)))
                        conc (eval-body (second (rest body)))]
                    (eval (conj (map #(list `quote %) (list prem conc)) `infer)))      
      'substitution (eval (conj (map #(list `quote (eval-body %)) (rest body)) `substitution)))
    body))

(defn create-item
  "Creates a new item from body and [optional] rule"
  ([body] (create-item body nil))
  ([body rule]
	  (let [newbody (eval-body body)]
     (if (vector? newbody)
       newbody
       {:id   (new-id)
        :body newbody
	      :rule rule}))))

(defn create-items
  "Consumes a collection of bodies and a [optional] rule and creates items for the internal proof structure"
  ([bodies] (create-items bodies nil))
  ([bodies rule]
    (let [newb (init-vars bodies)
          ;; to ensure that all bodies of all items are either symbols or lists, convert all lazy-seq (they come from rule evaluation) to lists
          non-lazy (clojure.walk/postwalk (fn [node]
                                            (if (instance? clojure.lang.LazySeq node)
                                              (apply list node)
                                              node)) newb)]
      (map #(create-item % rule) non-lazy))))
;; --------------------------------------------------------

;; functions for advancing the proofs state (steps, choose-options, rename-vars)
(defn check-args
  "Checks the arguments for errors and irregularities. 
If nothing is found returns a map with additional information for further proceeding."
  [proof rule lines forward?]
  (cond
    (not (log/rule-exist? rule))
    (throw (Exception. (str "A rule named \"" rule "\" does not exists.")))
    
    ;; for forward rules with no premises (e.g. equality introduction)
    (and (empty? lines)
         forward? 
         (zero? (log/rule-givens rule)))
    {:todo (first (filter #(= (:body %) :todo) (flatten proof)))
     :obligatories []
     :optional []}
    
    (not (apply distinct? lines))
    (throw (Exception. "There are duplicate lines in the arguments"))
    
    (some #(< % 1) (flatten lines))
    (throw (Exception. "There are no line numbers less than 1"))
                       
    (some #(> % (count (flatten proof))) (flatten lines))
    (throw (Exception. (str "There are no line numbers greater than " (count (flatten proof)))))
    
    :else
    (let [lastline    (last (sort-by #(if (vector? %) (first %) %) lines))
          items       (map #(get-item proof %) lines)
          obligatories (if forward? (get-proofed-items items) (get-unproofed-items items))
          optional     (if forward? (get-unproofed-items items) (get-proofed-items items))
          numObligatories (if forward? (log/rule-givens rule) (log/rule-conclusions rule)) 
          numOptionals    (dec (if forward? (log/rule-conclusions rule) (log/rule-givens rule)))
          scope (get-scope proof (get-item proof lastline))
          todos (filter #(= (:body %) :todo) scope)]
      (cond
        (not-every? #(contains? (set scope) %) items)
        (throw (Exception. "Not all lines are in the same scope"))

        (> (count todos) 1)
        (throw (Exception. "There can't be more than one \"...\" line inside your scope"))
        
        ;; only important for backward steps
        (and (not forward?)
             (< (count todos) 1))
        (throw (Exception. "There is no \"...\" line inside your scope to work towards to"))
        
        (some #(contains? (set items) %) todos)
        (throw (Exception. (str "Can't use a \"...\" line for " (if forward? "forward" "backward") " resulting")))
        
        (not= (count obligatories) numObligatories)
        (throw (Exception. (str (if (> (count obligatories) numObligatories)
                                   "Too many "
                                   "Not enough ") (if forward? 
                                                    "proofed lines (rule != nil)"
                                                    "unproofed lines (rule = nil)") " for this rule. You need exactly " numObligatories)))
        
        (> (count optional) numOptionals)
        (throw (Exception. (str "Too many [optional] " (if forward? 
                                                         "unproofed lines (rule = nil)"
                                                         "proofed lines (rule != nil)") " for this rule. You can have at a max " numOptionals)))
        
        :else {:todo (first todos)
               :obligatories obligatories 
               :optional optional}))))

;; TODO Check that only variables (e.g. V1 V2) can be renamed
(defn rename-var
  "Replaces inside the proof all instances of old with new"
  [proof old new]
  (check-duplicates
    (clojure.walk/postwalk
      (fn [node]
        (if (map? node)
          (cond
            (symbol? (:body node)) (if (= (:body node) old)
                                     (assoc node :body new) 
                                     node)
            (list? (:body node)) (assoc node :body (clojure.walk/prewalk-replace {old new} (:body node)))
            :else node)
          node)) 
      proof)))

(defn choose-option
  "Chooses option num on line to be inserted into proof.
In case there is nothing to choose or the num is invalid, it throws an exception."
  [proof line num]
  (let [item (get-item proof line)
        options (:body item)
        opt (get options num)]
    (cond 
      (not (map? options))
      (throw (Exception. (str "There is nothing to choose in line " line)))
      
      (nil? opt)
      (throw (Exception. (str "There is no option \"" num "\" to choose")))
      
      :else
      (let [items (if (vector? opt) opt [opt])
            new-items (create-items items (:rule item))
            p1 (reduce #(add-after-item %1 item %2) proof new-items)
            ;; adjust the ids of the proof to point on the newly created items instead of the old "choose-item" before checking for duplicates
            p2 (adjust-ids (with-meta p1 {(:id item) (apply list (map #(if (vector? %)
                                                                         [(:id (first %)) (:id (last %))]
                                                                         (:id %)) new-items))}))]
        (check-duplicates (remove-item p2 item))))))

;; TEST forward step, should also regard inner elements

;; maybe rules of the logic you're using have to be prepared to be used inside of a line
;; e.g. for temporal logik you have to ensure that input and output have the same timestamp (at x) and then remove it for later use
(defn prep-temporal
  [rule]
  (let [given      (first (:given rule))
        conclusion (first (:conclusion rule))]
    (if (and (= (first given)      'at)
             (= (first conclusion) 'at))
      (if (= (second given) (second conclusion))
        {:name       (:name rule)
         :given      [(first (drop 2 given))]
         :conclusion [(first (drop 2 conclusion))]}
        (throw (Exception. (str "The rule \"" rule "\" is not usable for an inside step due to the two different time points of the premise and the conclusion"))))
      rule)))

(defn step-f-inside
  [proof rule & lines]
  (cond
    (> (log/rule-givens rule) 1)
    (throw (Exception. (str "The rule " rule " needs more than 1 premise. Inside-Steps can only be executed with rules that need exactly 1 premise.")))
    
    (> (log/rule-conclusions rule) 1)
    (throw (Exception. (str "The rule " rule " has more than 1 conclusion. Inside-Steps only work with rules that have exactly 1 conclusion.")))
    
    (> (count lines) 1)
    (throw (Exception. "Too many arguments for an inside usable rule (max. 1)"))
    
    :else
    (let [info (check-args proof rule lines true)
          r (prep-temporal (log/get-rule rule))
          rule-exe (fn [node]
                     (let [res (apply log/apply-rule (conj [r true] [node] []))]
;                       (println node ":" res)
                       (if (empty? res)
                         node
                         (first res))))
          item (get-item proof (first lines))
          body (:body item)
          new-body (clojure.walk/postwalk rule-exe body)
          new-item {:id (new-id)
                    :body new-body
                    :rule (pr-str rule (list (:id item)))}]
      (if (= body new-body)
        (do
          (println "Inside-step hasn't changed anything")
          proof)
        (check-duplicates (add-after-item proof item new-item))))))


(defn classical-combine
  [form]
  (let [res (log/apply-classicals form)]
    (if (empty? res)
      form
      (first res))))

(defn classical
  [proof line]
  (let [item (get-item proof line)
        body (:body item)
        new-body (clojure.walk/postwalk
                   (fn [node] 
                     (if (list? node)
                       (classical-combine node)
                       node))
                   body)
        new-id (new-id)]
    (if (= body new-body)
      (do (println "\"Classical\" hasn't changed anything.") proof)
      (if (:rule item)
        (check-duplicates (add-after-item proof item {:id   new-id
                                                      :body new-body
                                                      :rule (str "\"classical\" (" (:id item) ")")}))
        (if (or (true? new-body)
                (true? (second (rest new-body))));; for temporal logic (at x true)
          (check-duplicates (replace-item proof item {:id   (:id item)
                                                      :body (:body item)
                                                      :rule (str "\"classical\"")}))
          (check-duplicates (add-before-item (replace-item proof item {:id   (:id item)
                                                                       :body (:body item)
                                                                       :rule (str "\"classical\" (" new-id ")")})
                                             item
                                             {:id   new-id
                                              :body new-body
                                              :rule nil})))))))

;(defn insert-theorem
;  [proof theorem]
;  (let [item (first (remove #(= (:rule %) :premise) proof))
;        theo (log/get-rule theorem)
;        new-item {:id (new-id)
;                  :body (apply list (concat '(impl) (:given theo) (:conclusion theo)))
;                  :rule (str "\"theorem " theorem "\"")}]
;  (check-duplicates (add-before-item proof item new-item))))
;
;(defn insert-formula
;  [proof form]
;  (let [f (if (contains? #{'at 'forall 'exists} (first form))
;            (first (drop 2 form))
;            form)
;        res (clojure.walk/postwalk
;              classical-combine
;              f)
;        item (first (remove #(= (:rule %) :premise) proof))]
;    (if (true? res)
;      (check-duplicates (add-before-item proof item {:id (new-id)
;                                                     :body form
;                                                     :rule "\"classical theorem\""}))
;      (throw (Exception. (str "The formula \"" form "\" doesn't convert to \"true\". Therefore it can not be inserted into the proof"))))))
                                    
;; TEST END


            
(defn step-f
  "Performs a forward step on proof by applying rule on the lines"
  [proof rule & lines]
  (let [info       (check-args proof rule lines true)
        todo-item        (:todo info)        
        obligatory-items (:obligatories info)
        optional-items   (:optional info)
        obligatory-ids   (map get-item-id obligatory-items)
        obligatory-args (into [] (map item-to-rule-arg obligatory-items))
        optional-args   (into [] (map item-to-rule-arg optional-items))        
        rule-result (apply log/apply-rule (conj [rule true] obligatory-args optional-args))]
    (if (empty? rule-result)
      (throw (Exception. (str "Incorrect parameters for the rule \"" rule "\". Please check the description.")))
      ;; add the used rule to the optional items
      (let [p1 (reduce #(replace-item %1 %2 {:id   (:id %2)
                                             :body (:body %2)
                                             :rule (pr-str rule obligatory-ids)}) proof optional-items)]
        (if (> (count rule-result) 1)
          ;; more than one possible result, the user has to decide which one fits his needs
          (add-before-item p1 
                           todo-item
                           {:id   (new-id)
                            :body (apply merge (map-indexed #(hash-map (inc %1) %2) rule-result))
                            :rule (pr-str rule obligatory-ids)})
          ;; only one possible result (which can contain several items to insert)
          (let [result (if (vector? (first rule-result)) (first rule-result) rule-result)               
                new-items (create-items result (pr-str rule obligatory-ids))]
            ;; if there is no empty line, insert everthing behind the last obligatory item
            (check-duplicates 
              (if todo-item
                (reduce #(add-before-item %1 todo-item %2) p1 new-items)
                (reduce #(add-after-item %1 (last obligatory-items) %2) p1 new-items)))))))))

(defn step-b
  "Performs a backward step on proof by applying rule on the lines"
  [proof rule & lines]
  (let [info (check-args proof rule lines false)
        todo-item        (:todo info)        
        obligatory-items (:obligatories info)
        optional-items   (:optional info)
        optional-ids     (map get-item-id optional-items)
        obligatory-args (into [] (map item-to-rule-arg obligatory-items))
        optional-args   (into [] (map item-to-rule-arg optional-items))
        rule-result (apply log/apply-rule (conj [rule false] obligatory-args optional-args ))]
     (cond
       (empty? rule-result)
       (throw (Exception. "Incorrect parameters for the given rule"))
      
       (> (count rule-result) 1)
       ;; more than one possible result, the user has to decide which one fits his needs
       (let [id (new-id)
             p1 (reduce #(replace-item %1 %2 {:id   (:id %2)
                                              :body (:body %2)
                                              :rule (pr-str rule (conj optional-ids id))}) proof obligatory-items)]
         (add-after-item p1 
                         todo-item
                         {:id   id
                          :body (apply merge (map-indexed #(hash-map (inc %1) %2) rule-result))
                          :rule nil}))
       :else
       ;; only one possible result (which can contain several items to insert)
       (let [result (if (vector? (first rule-result)) (first rule-result) rule-result)   
             new-items (create-items result)
             new-ids   (map get-item-id new-items)
             p1 (reduce #(replace-item %1 %2 {:id   (:id %2)
                                              :body (:body %2)
                                              :rule (pr-str rule (concat new-ids optional-ids))}) proof obligatory-items)
             ;; TEST 
             new-items-1 (filter #(or (vector? %)
                                      (not (nil? (:rule %)))) new-items)
             new-items-2 (remove #(or (vector? %)
                                      (not (nil? (:rule %)))) new-items)
             p2 (reduce #(add-after-item %1 todo-item %2) p1 new-items-2)]
         (check-duplicates (reduce #(add-before-item %1 todo-item %2) p2 new-items-1))))))
;; TEST END
;         (check-duplicates (reduce #(add-after-item %1 todo-item %2) p1 new-items))))))
;; -----------------------------------------------------------------------------
    