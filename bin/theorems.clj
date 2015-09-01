{:name "modus-tollens", 
 :given [(impl a b) (not b)], 
 :conclusion [(not a)], 
 :proof [{:rule :premise, :id 1, :body (impl a b)} 
         {:rule :premise, :id 2, :body (not b)} 
         [{:id 5, :body a, :rule :assumption} 
          {:id 8, :body b, :rule "\"impl-e\" (1 5)"} 
          {:id 9, :body contradiction, :rule "\"not-e\" (2 8)"}] 
         {:id 4, :body (not a), :rule "\"not-i\" ([5 9])"}]}
{:name "temporal-5", 
 :given [(at x (asap (not a)))], 
 :conclusion [(at x (not (asap a)))], 
 :proof [{:id 1, :body (at x (asap (not a))), :rule :premise} 
         {:id 11, :body (at y (not a)), :rule "\"asap-e\" (1)"} 
         [{:id 5, :body (at x (not (not (asap a)))), :rule :assumption} 
          {:id 8, :body (at x (asap a)), :rule "\"not-e\" (5)"} 
          {:id 12, :body (at y a), :rule "\"asap-e\" (8)"} 
          {:id 7, :body (at y (and a (not a))), :rule "\"and-i\" (12 11)"}] 
         {:id 4, :body (at x (not (not (not (asap a))))), :rule "\"not-i\" ([5 7])"} 
         {:id 3, :body (at x (not (asap a))), :rule "\"not-e\" (4)"}]}
{:name "temporal-6", :given [(at x (not (sometimes a)))], :conclusion [(at x (always (not a)))], :proof [{:id 1, :body (at x (not (sometimes a))), :rule :premise} [{:id 4, :body (<= x y), :rule :assumption} [{:id 10, :body (at y a), :rule :assumption} {:id 15, :body (at x (sometimes a)), :rule "\"sometimes-i\" (4 10)"} {:id 12, :body (at x (and (sometimes a) (not (sometimes a)))), :rule "\"and-i\" (15 1)"}] {:id 6, :body (at y (not a)), :rule "\"not-i\" ([10 12])"}] {:id 3, :body (at x (always (not a))), :rule "\"always-i\" ([4 6])"}]}
{:name "temporal-7", :given [(at x (not (always a)))], :conclusion [(at x (sometimes (not a)))], :proof [{:id 1, :body (at x (not (always a))), :rule :premise} [{:id 5, :body (at x (not (sometimes (not a)))), :rule :assumption} {:id 10, :body (at x (always a)), :rule "\"temporal-6\" (5)"} {:id 7, :body (at x (and (always a) (not (always a)))), :rule "\"and-i\" (10 1)"}] {:id 4, :body (at x (not (not (sometimes (not a))))), :rule "\"not-i\" ([5 7])"} {:id 3, :body (at x (sometimes (not a))), :rule "\"not-e\" (4)"}]}
{:name "temporal-1", :given [(at x a)], :conclusion [(at x (always a))], :proof [{:id 1, :body (at x a), :rule :premise} [{:id 4, :body (<= x x), :rule :assumption}] {:id 3, :body (at x (always a)), :rule "\"always-i\" ([4 1])"}]}
{:name "temporal-8" 
 :given [(at x (until a false))]
 :conclusion [(at x false)]
 :proof "not yet"}
;; TEST
{:name "temporal-8-in"
 :given [(until a false)]
 :conclusion [false]}
;; TEST
{:name "temporal-9", :given [(at x (sometimes a))], :conclusion [(at x (until true a))], :proof [{:id 1, :body (at x (sometimes a)), :rule :premise} {:id 11, :body (at y a), :rule "\"sometimes-e\" (1)"} {:id 10, :body (<= x y), :rule "\"sometimes-e\" (1)"} [{:id 5, :body (at x (not (until true a))), :rule :assumption} {:id 12, :body (at x (or (always (not a)) (until (not a) (and (not true) (not a))))), :rule "\"not-until\" (5)"} {:id 13, :body (at x (or (always (not a)) (until (not a) false))), :rule "\"classical\" (12)"} {:id 14, :body (at x (or (always (not a)) false)), :rule "\"temporal-8-in\" (13)"} {:id 15, :body (at x (always (not a))), :rule "\"classical\" (14)"} {:id 16, :body (at y (not a)), :rule "\"always-e\" (15 10)"} {:id 7, :body (at y (and a (not a))), :rule "\"and-i\" (11 16)"}] {:id 4, :body (at x (not (not (until true a)))), :rule "\"not-i\" ([5 7])"} {:id 3, :body (at x (until true a)), :rule "\"not-e\" (4)"}]}
{:name "temporal-10", :given [(at x (asap (sometimes a)))], :conclusion [(at x (sometimes a))], :proof [{:id 1, :body (at x (asap (sometimes a))), :rule :premise} {:id 6, :body (at x1 (sometimes a)), :rule "\"asap-e\" (1)"} {:id 7, :body (next x x1), :rule "\"asap-seriality\" ()"} {:id 8, :body (<= x x1), :rule "\"asap/<=\" (7)"} {:id 9, :body (<= x1 z), :rule "\"sometimes-e\" (6)"} {:id 10, :body (at z a), :rule "\"sometimes-e\" (6)"} {:id 11, :body (<= x z), :rule "\"transitivity\" (8 9)"} {:id 3, :body (at x (sometimes a)), :rule "\"sometimes-i\" (10 11)"}]}
{:name "temporal-10-in"
 :given [(asap (sometimes a))]
 :conclusion [(sometimes a)]}
{:name "blabla", :given [(at x false)], :conclusion [(at x false)], :proof [{:id 1, :body (at x false), :rule :premise}]}
{:name "c1", :given [], :conclusion [(at x (impl (not a) (not a)))], :proof [{:id 2, :body (at x (impl (not a) (not a))), :rule "\"classical\""}]}
{:name "rule-2", :given [(at x (impl a b))], :conclusion [(at x (impl (not b) (not a)))], :proof [{:id 1, :body (at x (impl a b)), :rule :premise} [{:id 4, :body (at x (not b)), :rule :assumption} [{:id 7, :body (at x a), :rule :assumption} {:id 10, :body (at x b), :rule "\"impl-e\" (1 7)"} {:id 9, :body (at x (and b (not b))), :rule "\"and-i\" (10 4)"}] {:id 6, :body (at x (not a)), :rule "\"not-i\" ([7 9])"}] {:id 3, :body (at x (impl (not b) (not a))), :rule "\"impl-i\" ([4 6])"}]}
{:name "rule-3", :given [(at x (impl a b)) (at x (impl b c))], :conclusion [(at x (impl a c))], :proof [{:rule :premise, :id 1, :body (at x (impl a b))} {:rule :premise, :id 2, :body (at x (impl b c))} [{:id 5, :body (at x a), :rule :assumption} {:id 8, :body (at x b), :rule "\"impl-e\" (1 5)"} {:id 9, :body (at x c), :rule "\"impl-e\" (2 8)"}] {:id 4, :body (at x (impl a c)), :rule "\"impl-i\" ([5 9])"}]}
{:name "teo-5", :given [], :conclusion [(at x (impl (asap (not a)) (not (asap a))))], :proof [[{:id 3, :body (at x (asap (not a))), :rule :assumption} {:id 6, :body (at x1 (not a)), :rule "\"asap-e\" (3)"} [{:id 8, :body (at x (not (not (asap a)))), :rule :assumption} {:id 13, :body (at x (asap a)), :rule "\"not-e\" (8)"} {:id 14, :body (at x1 a), :rule "\"asap-e\" (13)"} {:id 10, :body (at x1 (and a (not a))), :rule "\"and-i\" (14 6)"}] {:id 7, :body (at x (not (not (not (asap a))))), :rule "\"not-i\" ([8 10])"} {:id 5, :body (at x (not (asap a))), :rule "\"not-e\" (7)"}] {:id 2, :body (at x (impl (asap (not a)) (not (asap a)))), :rule "\"impl-i\" ([3 5])"}]}
{:name "teo-4", :given [], :conclusion [(at x (impl (and true (asap (not a))) (asap (not a))))], :proof [[{:id 3, :body (at x (and true (asap (not a)))), :rule :assumption} {:id 6, :body (at x (asap (not a))), :rule "\"classical\" (3)"}] {:id 2, :body (at x (impl (and true (asap (not a))) (asap (not a)))), :rule "\"impl-i\" ([3 6])"}]}
{:name "c8-1", :given [], :conclusion [(at x (impl (and a (asap false)) false))], :proof [{:id 2, :body (at x (impl (and a (asap false)) false)), :rule "\"classical\""}]}
{:name "c8-2", :given [], :conclusion [(at x (impl false false))], :proof [{:id 2, :body (at x (impl false false)), :rule "\"classical\""}]}
{:name "test1", :given [(at x (and a b))], :conclusion [(at x (and b a))], :proof [{:id 1, :body (at x (and a b)), :rule :premise} {:id 4, :body (at x a), :rule "\"and-e1\" (1)"} {:id 5, :body (at x b), :rule "\"and-e2\" (1)"} {:id 7, :body (at x (and b a)), :rule "\"and-i\" (4 5)"}]}
