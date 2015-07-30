{:name "modus-tollens", 
 :given [(impl a b) (not b)], 
 :conclusion [(not a)], 
 :proof [{:rule :premise, :id 1, :body (impl a b)} 
         {:rule :premise, :id 2, :body (not b)} 
         [{:id 5, :body a, :rule :assumption} 
          {:id 8, :body b, :rule "\"impl-e\" (1 5)"} 
          {:id 9, :body contradiction, :rule "\"not-e\" (2 8)"}] 
         {:id 4, :body (not a), :rule "\"not-i\" ([5 9])"}]}
