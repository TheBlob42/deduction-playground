(ns deduction-playground.signature)

; Signature in Group-Form
(def sign {:const '[c d e]
           :func  '[f 1 g 2]
           :pred  '[P 1 Q 2]})

;; Signature in Elements-Form would be
;; (def sign-e {:c [:const 0]
;;              :d [:const 0]
;;              :e [:const 0]
;;              :f [:func 1]
;;              :g [:func 2]
;;              :P [:pred 1]
;;              :Q [:pred 2]})

(defn signature
  "Converts a signature from group-form into elements-form"
  [sign]
  (let [constants  (:const sign)
        functions  (:func sign)
        predicates (:pred sign)]
    (if (apply distinct? (concat constants (take-nth 2 functions) (take-nth 2 predicates)))
      (let [cmap (into {} (map #(hash-map (keyword %) [:const 0]) constants))
            fmap (into {} (map #(hash-map (keyword (first %)) [:func (second %)]) (partition 2 functions)))
            pmap (into {} (map #(hash-map (keyword (first %)) [:pred (second %)]) (partition 2 predicates)))]
        (merge cmap fmap pmap))
      ;else
      (throw (Exception. "The given signature has duplicate identifiers")))))


