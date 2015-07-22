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
 :conclusion '[a]}
{:name "efq"
 :given      '[contradiction]
 :conclusion '[a]}
          
; EQUAL
{:name "equal-i"
 :given      '[]
 :conclusion '[(= t t)]}
{:name "equal-e"
 :given      '[(= a b)
               (substitution phi x a)]
 :conclusion '[(substitution phi x b)]}
          
; FORALL 
{:name "forall-i"
 :given      '[(infer (actual x0)
                      (substitution phi x x0))]
 :conclusion '[(forall [x] phi)]}
{:name "forall-e"
 :given      '[(forall [x] phi)]
 :conclusion '[(substitution phi x t)]}; müsste es "(actual t)" sein?

; EXISTS
{:name "exists-i"
 :given      '[(substitution phi x t)]
 :conclusion '[(exists [x] phi)]};müsste es "(actual t)" sein?
{:name "exists-e"
 :given      '[(exists [x] phi)
               (infer [(actual x0)
                       (substitution phi x x0)]
                      X)]
 :conclusion '[X]}
          
; TEST
{:name "notnot-e"
 :given '[(not (not a))]
 :conclusion '[a]}

; MULTIPLE CONCLUSIONS TESTING
{:name "test"
 :given '[(and a b)]
 :conclusion '[a b]}

{:name "multi"
 :given '[a b]
 :conclusion '[(not a) (or a b)]}

{:name "testb"
 :given '[a (not a)]
 :conclusion '[(and a b)]}

{:name "or-e-backwards"
 :given '[X]
 :conclusion '[(or a b)
               (infer a X)
               (infer b X)]}




