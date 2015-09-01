;; AND
{:name "and-i"
 :given [(at i a) (at i b)]
 :conclusion [(at i (and a b))]}
{:name "and-e1"
 :given [(at i (and a b))]
 :conclusion [(at i a)]}
{:name "and-e2"
 :given [(at i (and a b))]
 :conclusion [(at i b)]}

;; OR
{:name "or-i1"
 :given [(at i a)]
 :conclusion [(at i (or a b))]}
{:name "or-i2"
 :given [(at i b)]
 :conclusion [(at i (or a b))]}
{:name "or-e1"
 :given [(at i (or a b)) (at i (not a))]
 :conclusion [(at i b)]}
;; because of the nested structure of "(at i (or a b))" we need a rule for each side (a, b)
{:name "or-e2"
 :given [(at i (or a b)) (at i (not b))]
 :conclusion [(at i a)]}
{:name "not-or"
 :given [(at i (not (or a b)))]
 :conclusion [(at i (and (not a) (not b)))]}

;; NOT
{:name "not-i"
 :given [(infer (at j c) (at i (and b (not b))))]
 :conclusion [(at j (not c))]}
{:name "not-e"
 :given [(at i (not (not a)))]
 :conclusion [(at i a)]}

;; IMPL
{:name "impl-i"
 :given [(infer (at i c) (at i b))]
 :conclusion [(at i (impl c b))]}
{:name "impl-e"
 :given [(at i (impl a b)) (at i a)]
 :conclusion [(at i b)]}

;; ASAP
{:name "asap-i"
 :given [(at j a) (next i j)]
 :conclusion [(at i (asap a))]}
{:name "asap-e"
 :given [(at i (asap a))]
 :conclusion [(at j a)]}

;; ALWAYS
{:name "always-i"
 :given [(infer (<= i j) (at j a))]
 :conclusion [(at i (always a))]}
{:name "always-e" 
 :given [(<= i j) (at i (always a))]
 :conclusion [(at j a)]}

;; is proofable as theorem
;{:name "generalisation"
; :given [(at x a)]
; :conclusion [(at x (always a))]}

;; SOMETIMES
{:name "sometimes-i"
 :given [(at j a) (<= i j)]
 :conclusion [(at i (sometimes a))]}
{:name "sometimes-e"
 :given [(at i (sometimes a))]
 :conclusion [(<= i j) (at j a)]}
{:name "not-sometimes"
 :given [(at i (not (sometimes a)))]
 :conclusion [(at i (always (not a)))]}

;; UNTIL
{:name "until-i"
 :given [(at i b)]
 :conclusion [(at i (until a b))]}
{:name "until-e"
 :given [(at i (always (impl b c)))
          (at i (always (impl (and a (asap c)) c)))]
 :conclusion [(at i (impl (until a b) c))]}
{:name "not-until"
 :given [(at i (not (until a b)))]
 :conclusion [(at i (or (always (not b))
                         (until (not b) 
                                (and (not a) (not b)))))]}

;; RELATIONAL JUDGEMENTS
{:name "reflexivity"
 :given []
 :conclusion [(<= i j)]}
{:name "asap-seriality"
 :given []
 :conclusion [(next i j)]}
{:name "</<="
 :given [(< i j)]
 :conclusion [(<= i j)]}
{:name "asap/<="
 :given [(next i j)]
 :conclusion [(<= i j)]}
{:name "transitivity"
 :given [(<= i j) (<= j k)]
 :conclusion [(<= i k)]}
{:name "linearity"
 :given [(<= i j) (<= i k)]
 :conclusion [(or (or (<= j k) (j ~= k)) (<= k j))]}
