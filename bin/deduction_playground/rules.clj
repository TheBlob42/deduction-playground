; AND
{:name "and-i"
 :given      '[a b]
 :conclusion '[(and a b)]}
{:name "and-e1"
 :given      '[(and a b)]
 :conclusion '[a]}
{:name "and-e2"
 :given      '[(and a b)]
 :conclusion '[b]}

; OR
{:name "or-i1"
 :given      '[a]
 :conclusion '[(or a b)]}
{:name "or-i2"
 :given      '[a]
 :conclusion '[(or b a)]}
{:name "or-e"
 :given      '[(or a b)
               (infer a X)
               (infer b X)]
 :conclusion '[X]}

; IMPL
{:name "impl-i"
 :given      '[(infer a b)]
 :conclusion '[(impl a b)]}
{:name "impl-e"
 :given      '[a (impl a b)]
 :conclusion '[b]}

; NOT
{:name "not-i"
 :given      '[(infer a contradiction)]
 :conclusion '[(not a)]}
{:name "not-e"
 :given      '[a (not a)]
 :conclusion '[contradiction]}

; RAA/CONTRA
{:name "raa"
 :given      '[(infer (not a) contradiction)]
 :conclusion '[a]};TODO
{:name "efq"
 :given      '[contradiction]
 :conclusion '[a]};maybe TODO
          
; EQUAL
{:name "equal-i"
 :given      '[]
 :conclusion '[(= t t)]}
{:name "equal-e"
 :given      '[(= a b)
               (substitution phi a x)]
 :conclusion '[(substitution phi b x)]}
          
; FORALL 
{:name "forall-i"
 :given      '[(infer (actual x0)
                      (substitution phi x0 x))]
 :conclusion '[(forall [x] phi)]};TODO (actual)
{:name "forall-e"
 :given      '[(forall [x] phi)]
 :conclusion '[(substitution phi t x)]};TODO

; EXISTS
{:name "exists-i"
 :given      '[(substitution phi t x)]
 :conclusion '[(exists [x] phi)]};TODO
{:name "exists-e"
 :given      '[(exists [x] phi)
               (infer [(actual x0)
                       (substitution phi x0 x)]
                      X)]
 :conclusion '[X]};TODO
          




