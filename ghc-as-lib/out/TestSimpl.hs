{-# OPTIONS_GHC -Wno-typed-holes #-}
module TestSimpl where 

-------------------------------------
-- TEST INLINING --
-------------------------------------

{- dupli [] = []
dupli (x:xs) = dup ++ dupli xs 
    where dup = [x,x] -} 

-- here it is a difference between compiling to core and simpl
-- this is simplified core, dup is inlined (and we have a Rec binder). The simple optimiser (in desugar step) also performs inlining 
{- CompToSimpl 
[Rec [(dupli,Lam a (Lam ds (Case (Var ds) wild (TyConApp [] [TyVarTy a]) 
     [Alt (DataAlt DataCon []) [] (App (Var []) (Type (TyVarTy a))),
      Alt (DataAlt DataCon :) [x,xs] (App (App (App (Var ++) (Type (TyVarTy a))) 
        (App (App (App (Var :) (Type (TyVarTy a))) (Var x)) (App (App (App (Var :) 
            (Type (TyVarTy a))) (Var x)) (App (Var []) (Type (TyVarTy a)))))) 
            (App (App (Var dupli) (Type (TyVarTy a))) (Var xs)))])))],

NonRec $trModule4 (Lit main),NonRec $trModule3 (App (Var TrNameS) (Var $trModule4)),NonRec $trModule2 (Lit TestSimpl),NonRec $trModule1 (App (Var TrNameS) (Var $trModule2)),NonRec $trModule (App (App (Var Module) (Var $trModule3)) (Var $trModule1))]
-}
{- CompToCore (Non rec, recursive let inside)
[NonRec dupli (Lam a (Let (
        Rec [(dupli,Lam ds 
                    (Case (Var ds) wild (TyConApp [] [TyVarTy a]) 
                        [Alt (DataAlt DataCon []) [] (App (Var []) (Type (TyVarTy a))),
                        Alt (DataAlt DataCon :) [x,xs] (App (App (App (Var ++) (Type (TyVarTy a))) 
                        (App (App (App (Var :) (Type (TyVarTy a))) (Var x)) (App (App (App (Var :) 
                        (Type (TyVarTy a))) (Var x)) (App (Var []) (Type (TyVarTy a)))))) 
                        (App (Var dupli) (Var xs)))]))]) (Var dupli))),

NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit TestSimpl)))]
-}



{- dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs  -}

{- After Desugarer (before simplifier) equal to the definition with where 
[NonRec dupli (Lam a (Let 
    (Rec [(dupli,Lam ds (Case (Var ds) wild (TyConApp [] [TyVarTy a]) 
        [Alt (DataAlt DataCon []) [] (App (Var []) (Type (TyVarTy a))),
        Alt (DataAlt DataCon :) [x,xs] (App (App (App (Var ++) (Type (TyVarTy a))) 
        (App (App (App (Var :) (Type (TyVarTy a))) (Var x)) (App (App (App (Var :) 
        (Type (TyVarTy a))) (Var x)) (App (Var []) (Type (TyVarTy a)))))) 
        (App (Var dupli) (Var xs)))]))]) (Var dupli))),
    
NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit TestSimpl)))]
-}

 
{- After simplifier, equal to the simplified version with where dup = ... 
[Rec [(dupli,Lam a (Lam ds (Case (Var ds) wild (TyConApp [] [TyVarTy a]) 
    [Alt (DataAlt DataCon []) [] (App (Var []) (Type (TyVarTy a))),
     Alt (DataAlt DataCon :) [x,xs] (App (App (App (Var ++) (Type (TyVarTy a))) 
        (App (App (App (Var :) (Type (TyVarTy a))) (Var x)) (App (App (App (Var :) 
            (Type (TyVarTy a))) (Var x)) (App (Var []) (Type (TyVarTy a)))))) 
            (App (App (Var dupli) (Type (TyVarTy a))) (Var xs)))])))],
            
NonRec $trModule4 (Lit main),NonRec $trModule3 (App (Var TrNameS) (Var $trModule4)),NonRec $trModule2 (Lit TestSimpl),NonRec $trModule1 (App (Var TrNameS) (Var $trModule2)),NonRec $trModule (App (App (Var Module) (Var $trModule3)) (Var $trModule1))]
-}

-- Can we match this as welL?

{- dupli [] = []
dupli (x:xs) = dup ++ dupli xs 
    where dup = _ -} 

-- ++ seems to be removed by simplifier... 
-- cannot match directly, also this is now a NonRec, and not a Rec (recursive) function anymore. 
-- I dont get why the recursive case seems to be subsumed by the hole. 
{-
[NonRec dupli (Lam a (Lam a1 (Lam ds (Case (Var ds) wild (TyConApp [] [TyVarTy a1]) 
    [Alt (DataAlt DataCon []) [] (App (Var []) (Type (TyVarTy a1))),
    Alt (DataAlt DataCon :) [x,xs] 
-    (Case (App (App (App (Var typeError) 
-    (Type (TyConApp BoxedRep [TyConApp Lifted []]))) (Type (TyConApp () []))) (Lit src/TestSimpl.hs:42:17: error:)) 
-    wild1 
-    (TyConApp [] [TyVarTy a1]) [])])))),
    
NonRec $trModule4 (Lit main),
NonRec $trModule3 (App (Var TrNameS) (Var $trModule4)),
NonRec $trModule2 (Lit TestSimpl),
NonRec $trModule1 (App (Var TrNameS) (Var $trModule2)),
NonRec $trModule (App (App (Var Module) (Var $trModule3)) (Var $trModule1))]
-}

-- compare with desugared, here we also get a NonRec but with a recursive let inside instead.
-- but here you can actually see the recursive call, compared to the simplified one. 
-- here ++ is still present (and the expression has another structure, but that would be the same without the hole.)
{-
[NonRec dupli (Lam a (Lam a (Let 
    (Rec [(dupli,Lam ds (Case (Var ds) wild (TyConApp [] [TyVarTy a]) 
        [Alt (DataAlt DataCon []) [] (App (Var []) (Type (TyVarTy a))),
         Alt (DataAlt DataCon :) [x,xs] (App (App (App (Var ++) (Type (TyVarTy a))) 
          (Case (App (App (App (Var typeError) (Type (TyConApp BoxedRep [TyConApp Lifted []]))) (Type (TyConApp () []))) (Lit src/TestSimpl.hs:42:17: error:))
           wild 
          (TyConApp [] [TyVarTy a]) [])) (App (Var dupli) (Var xs)))]))]) (Var dupli)))),
          
NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit TestSimpl)))]
-}

-- =======================================================================
 {- 
dupli [] = []
dupli (x:xs) = _ ++ dupli xs -}


-- same problem here, where does the recursive call go?? 
{- CompToSimpl
[NonRec dupli (Lam a (Lam a1 (Lam ds 
    (Case (Var ds) wild (TyConApp [] [TyVarTy a1]) 
        [Alt (DataAlt DataCon []) [] (App (Var []) (Type (TyVarTy a1))),
        Alt (DataAlt DataCon :) [x,xs] 
            (Case (App (App (App (Var typeError) (Type (TyConApp BoxedRep [TyConApp Lifted []]))) 
                (Type (TyConApp () []))) (Lit src/TestSimpl.hs:82:16: error:)) 
                wild1 
                (TyConApp [] [TyVarTy a1]) [])])))),

NonRec $trModule4 (Lit main),
NonRec $trModule3 (App (Var TrNameS) (Var $trModule4)),
NonRec $trModule2 (Lit TestSimpl),
NonRec $trModule1 (App (Var TrNameS) (Var $trModule2)),
NonRec $trModule (App (App (Var Module) (Var $trModule3)) (Var $trModule1))]
-}

{- CompToCore
[NonRec dupli (Lam a (Lam a (Let 
    (Rec [(dupli,Lam ds 
        (Case (Var ds) wild (TyConApp [] [TyVarTy a]) 
        [Alt (DataAlt DataCon []) [] (App (Var []) (Type (TyVarTy a))),
        Alt (DataAlt DataCon :) [x,xs] (App (App (App (Var ++) (Type (TyVarTy a))) (
            Case (App (App (App (Var typeError) (Type (TyConApp BoxedRep [TyConApp Lifted []]))) 
            (Type (TyConApp () []))) (Lit src/TestSimpl.hs:82:16: error:)) 
            wild 
            (TyConApp [] [TyVarTy a]) [])) (App (Var dupli) (Var xs)))]))]) (Var dupli)))),
            
NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit TestSimpl)))]
-}
-- ====================================================================================================

{- dupli :: [a] -> [a]
dupli xs = concatMap (replicate 2) xs  -}

{-
[NonRec dupli (Lam a_a15ql (Lam xs_a15q8 (App (App (App (App (App (App (Var concatMap) 
    (Type (TyConApp [] []))) (Type (TyVarTy a_a15ql))) (Type (TyVarTy a_a15ql))) (Var $fFoldable[]))
    (App (App (Var replicate) (Type (TyVarTy a_a15ql))) (App (Var I#) (Lit 2)))) (Var xs_a15q8)))),
    
NonRec $trModule1_r15pC (Lit main),NonRec $trModule2_r15qA (App (Var TrNameS) (Var $trModule1_r15pC)),NonRec $trModule3_r15qB (Lit TestSimpl),NonRec $trModule4_r15qC (App (Var TrNameS) (Var $trModule3_r15qB)),NonRec $trModule (App (App (Var Module) (Var $trModule2_r15qA)) (Var $trModule4_r15qC))]
-}
{- dupli :: [a] -> [a]
dupli = concatMap (replicate 2) -}

{-
[NonRec dupli (Lam a_a16ex (App (App (App (App (App (Var concatMap) 
    (Type (TyConApp [] []))) (Type (TyVarTy a_a16ex))) (Type (TyVarTy a_a16ex))) 
    (Var $fFoldable[])) (App (App (Var replicate) (Type (TyVarTy a_a16ex))) (App (Var I#) (Lit 2))))),
    
NonRec $trModule1_r16dK (Lit main),NonRec $trModule2_r16eM (App (Var TrNameS) (Var $trModule1_r16dK)),NonRec $trModule3_r16eN (Lit TestSimpl),NonRec $trModule4_r16eO (App (Var TrNameS) (Var $trModule3_r16eN)),NonRec $trModule (App (App (Var Module) (Var $trModule2_r16eM)) (Var $trModule4_r16eO))]
-}

{- dupli :: [a] -> [a]
dupli xs = concatMap _ xs  -}

--- simpl
{-
[NonRec dupli (Lam a_a1aWw (Lam xs_a1aWj (App (App (App (App (App (App (Var concatMap)
    (Type (TyConApp [] []))) (Type (TyVarTy a_a1aWw))) (Type (TyVarTy a_a1aWw))) (Var $fFoldable[])) 
    (Case (App (App (App (Var typeError) (Type (TyConApp BoxedRep [TyConApp Lifted []]))) (Type (TyConApp () []))) 
    (Lit src/TestSimpl.hs:34:22: error:)) 
    wild_00 
    (FunTy {ft_af = AnonArgFlag, ft_mult = TyConApp Many [], ft_arg = TyVarTy a_a1aWw, ft_res = TyConApp [] [TyVarTy a_a1aWw]}) [])) 
    (Var xs_a1aWj)))),NonRec $trModule1_r1b0V (Lit main),

NonRec $trModule2_r1cl0 (App (Var TrNameS) (Var $trModule1_r1b0V)),NonRec $trModule3_r1cl1 (Lit TestSimpl),NonRec $trModule4_r1cl2 (App (Var TrNameS) (Var $trModule3_r1cl1)),NonRec $trModule (App (App (Var Module) (Var $trModule2_r1cl0)) (Var $trModule4_r1cl2))]-}

-- repHoles of simpl
{-
[NonRec dupli (Lam a_a1aWw (Lam xs_a1aWj (App (App (App (App (App (App (Var concatMap)
    (Type (TyConApp [] []))) (Type (TyVarTy a_a1aWw))) (Type (TyVarTy a_a1aWw))) (Var $fFoldable[]))
    (Var HOLE_h1)) (Var xs_a1aWj)))), 
    
NonRec $trModule1_r1b0V (Lit main),NonRec $trModule2_r1cl0 (App (Var TrNameS) (Var $trModule1_r1b0V)),NonRec $trModule3_r1cl1 (Lit TestSimpl),NonRec $trModule4_r1cl2 (App (Var TrNameS) (Var $trModule3_r1cl1)),NonRec $trModule (App (App (Var Module) (Var $trModule2_r1cl0)) (Var $trModule4_r1cl2))]
-}

{- dupli :: [a] -> [a]
dupli = concatMap _  -}

-- simpl 
{-
[NonRec dupli (Lam a_a1h42 (App (App (App (App (App (Var concatMap) 
    (Type (TyConApp [] []))) (Type (TyVarTy a_a1h42))) (Type (TyVarTy a_a1h42))) 
    (Var $fFoldable[])) (Case (App (App (App (Var typeError) (Type (TyConApp BoxedRep [TyConApp Lifted []]))) 
    (Type (TyConApp () []))) (Lit src/TestSimpl.hs:58:19: error:)) 
    wild_00 
    (FunTy {ft_af = AnonArgFlag, ft_mult = TyConApp Many [], ft_arg = TyVarTy a_a1h42, ft_res = TyConApp [] [TyVarTy a_a1h42]}) []))),
    
NonRec $trModule1_r1h8o (Lit main),NonRec $trModule2_r1isn (App (Var TrNameS) (Var $trModule1_r1h8o)),NonRec $trModule3_r1iso (Lit TestSimpl),NonRec $trModule4_r1isp (App (Var TrNameS) (Var $trModule3_r1iso)),NonRec $trModule (App (App (Var Module) (Var $trModule2_r1isn)) (Var $trModule4_r1isp))]
-}

-- repholes simpl 
{-
[NonRec dupli (Lam a_a1nb3 (App (App (App (App (App (Var concatMap) 
    (Type (TyConApp [] []))) (Type (TyVarTy a_a1nb3))) (Type (TyVarTy a_a1nb3)))
    (Var $fFoldable[])) (Var HOLE_h1))),
     
NonRec $trModule1_r1nfp (Lit main),NonRec $trModule2_r1ozo (App (Var TrNameS) (Var $trModule1_r1nfp)),NonRec $trModule3_r1ozp (Lit TestSimpl),NonRec $trModule4_r1ozq (App (Var TrNameS) (Var $trModule3_r1ozp)),NonRec $trModule (App (App (Var Module) (Var $trModule2_r1ozo)) (Var $trModule4_r1ozq))]
-}

----------------------------------------------------
 -- TEST ETA-reduction
----------------------------------------------------
-- HIGHER-ORDER FUNCS + EXPLICIT TYPING 

{- f :: (Int -> Int) -> [Int] -> [Int]
f g xs = map g xs 
 -}
-- simpl AST for f 
{-
Eta-reduced!
[NonRec f (App (App (Var map) (Type (TyConApp Int []))) (Type (TyConApp Int []))),

NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit TestSimpl)))]
-}

{- f :: (Int -> Int) -> [Int] -> [Int]
f = map  -}

-- same as the above!!
{- [NonRec f (App (App (Var map) (Type (TyConApp Int []))) (Type (TyConApp Int []))),

NonRec $trModule4 (Lit main),NonRec $trModule3 (App (Var TrNameS) (Var $trModule4)),NonRec $trModule2 (Lit TestSimpl),NonRec $trModule1 (App (Var TrNameS) (Var $trModule2)),NonRec $trModule (App (App (Var Module) (Var $trModule3)) (Var $trModule1))]
-}

-- NO HOF + EXPLICIT TYPES

{- f :: (Int -> Int) -> Int -> Int 
f g x = g x  -}

{- this is NOT eta reduced (why?)
[NonRec f (Lam g (Lam x (App (Var g) (Var x)))),

NonRec $trModule4 (Lit main),NonRec $trModule3 (App (Var TrNameS) (Var $trModule4)),NonRec $trModule2 (Lit TestSimpl),NonRec $trModule1 (App (Var TrNameS) (Var $trModule2)),NonRec $trModule (App (App (Var Module) (Var $trModule3)) (Var $trModule1))]
-}


{- f :: (Int -> Int) -> Int -> Int 
f g = g   -}
-- NOt eta reduced (not possible)
{-
[NonRec f (Lam g (Var g)),

NonRec $trModule1 (Lit main),NonRec $trModule2 (App (Var TrNameS) (Var $trModule1)),NonRec $trModule3 (Lit TestSimpl),NonRec $trModule4 (App (Var TrNameS) (Var $trModule3)),NonRec $trModule (App (App (Var Module) (Var $trModule2)) (Var $trModule4))]
-}

-- HOF + generic (but restrictive) typing

{- f :: (a -> a) -> [a] -> [a]
f g xs = map g xs 
 -}
-- NOT eta-reduced
{-
[NonRec f (Lam a (Lam g (Lam xs (App (App (App (App (Var map) (Type (TyVarTy a))) (Type (TyVarTy a))) (Var g)) (Var xs))))),

NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit TestSimpl)))]
-}

-- HOF + no typing (or ghc-inferred types)
{- f :: (a -> b) -> [a] -> [b]
f g xs = map g xs  -}

-- ETA reduced! Already after desugar, unchanged by simplifier. 
{-
[NonRec f (Var map),

NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit TestSimpl)))]
-}

{- dupli :: [a] -> [a] 
dupli xs = concatMap (replicate 2) xs  -}

-- Cannot be eta-reduced. It seems like functions that have an outer type-lambda in Core is not eta-reduced. 
-- it does not matter if dupli is given a type signature or not. Even though once again its recognised on 
-- HsSyn level by Hlint.

{- f :: a -> [a] -> [a]
f = (:) -}

{- -- variable names are introduced, (beta expansion)
[NonRec f (Lam a (Lam ds (Lam ds (App (App (App (Var :) 
(Type (TyVarTy a))) (Var ds)) (Var ds))))),

NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit ))) (App (Var TrNameS) (Lit )))]
-}

{- f :: a -> [a] -> [a]
f x xs = (:) x xs  -}

{- (no eta reduction), but the eta reduced version was beta-abstracted hence they would be matchable.
[NonRec f (Lam a (Lam x (Lam xs (App (App (App (Var :)
 (Type (TyVarTy a))) (Var x)) (Var xs))))),

NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit ))) (App (Var TrNameS) (Lit )))]
-}