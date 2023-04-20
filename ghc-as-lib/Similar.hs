{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isJust" #-}
module Similar where


import GHC.Core ( Expr(..), CoreExpr, CoreBndr, Bind(..), CoreProgram, Alt(..), AltCon(..))
import GHC.Unit.Types
import GHC.Types.Var (Var(..), isTyVar, tyVarKind, isTcTyVar, isId)
import GHC.Core.TyCo.Rep (Type(..), Kind, CoercionR, TyLit (StrTyLit), AnonArgFlag (VisArg))
import GHC.Core.Type (eqType)
import GHC.Types.Name (getOccString, occNameString)
import GHC.Data.FastString (fsLit)

import Debug.Trace ( trace )
import GHC.Core.TyCon (TyCon)
import Control.Monad (when)
import Data.Maybe (isNothing)
import GHC.Core.DataCon (DataCon(..), dataConName)
import GHC.Utils.Outputable (showSDocUnsafe, Outputable (ppr))
import GHC.Types.Literal (Literal(..), LitNumType)

import Utils ( isHoleVar, sp, isHoleExpr, isHoleVarExpr) 
import GHC.Cmm (isAssociativeMachOp)
import GHC.Core.Coercion (eqCoercion)

class Similar a where
    (~==) :: a -> a -> Bool

instance Similar CoreProgram where
    (x:xs) ~== (y:ys) = x ~== y && xs ~== ys
    [] ~== []         = True
    _ ~== _           = trace "different number of binders" False 

--- Without trace -------------------------------------
instance Similar (Bind Var) where
    (Rec es) ~== (Rec es') = all (\((b,e),(b',e')) -> b ~== b' && e ~== e') (zip es es')
                                    --and $ concatMap (\(x,x') -> map (\(y,y') -> x ~== y && x' ~== y') es) es'
    (NonRec v e) ~== (NonRec v' e') = v ~== v' && e ~== e'
    x ~== y  = case (x,y) of 
        (NonRec v e,Rec [(v',e')])  -> v ~== v' && e ~== e' 
        (Rec [(v',e')],NonRec v e)  -> v ~== v' && e ~== e'
        (_,_)                       -> False 


instance Similar (Expr Var) where
    (Var id) ~== (Var id')                  = --trace ("ID : id1: " ++ show id ++ "id2: " ++ show id' )
                                              id ~== id'
    (Type t) ~== (Type t')                  = --trace ("TYPES : t1: " ++ show t ++ " t2: " ++ show t' ) 
                                              t ~== t'
    (Lit l)  ~== (Lit l')                   = --trace ("LIT" `sp` show l `sp` show l')
                                              l ~== l' -- No point in checking equality of literals, they will not be equal
    (App e arg) ~== (App e' arg')           = --trace "APP" 
                                              e ~== e' && arg ~== arg'

                                             
    (Lam b e) ~== (Lam b' e')               = --trace "LAM" 
                                              b ~== b' && e ~== e' -- check that the type of the head is equal                                      
    (Case e v t as) ~== (Case e' v' t' as') = --trace "CASE" 
                                              t ~== t' && e ~== e' && as ~== as' 
    (Cast e co) ~== (Cast e' co')           = --trace "CAST" 
                                              co ~== co' && e ~== e'
    (Let b e)   ~== (Let b' e')             = --trace "LET" 
                                              b ~== b' && e ~== e'
    (Coercion c) ~== (Coercion c')          = --trace "COERCION"
                                              c ~== c' 
    x ~== y                                 | isHoleVarExpr x || isHoleVarExpr y = --trace ("isHole:" ++ "x: " ++ show x ++ "y: " ++ show y) 
                                                                       True -- if hole replaced with hole variable
                                            | isHoleExpr x || isHoleExpr y =  -- if before repHoles pass 
                                                                            True 
                                            | otherwise = --trace ("OTHER: X:" `sp` show x `sp` " Y: " `sp` show y)
                                                          False

{- we need checks for e.g  xs == reverse xs ~== reverse xs == xs 
however, pattern matching like below does not scale well at all. 
Need a clever approach. 
(App f@(App op e1) e2) ~== (App f'@(App op' e1') e2') 
                                                | isCommutative op =  trace ("op1" ++ show op ++ " op2:" ++ show op') $ 
                                                                op ~== op' && e1 ~== e1' || e2 ~== e2' || e1 ~== e2' && e2 ~== e1' 
                                                | otherwise = f ~== f' && e2 ~== e2' 
isCommutative :: CoreExpr -> Bool 
-- | Check if we are applying something commutative, then the order of the arguments are irrelevant
-- not sure how to check this, functions are just variables
isCommutative (Var op) = getOccString op `elem` ["==", "+", "*"] -- just for now
isCommutative _ = False  -}

instance Similar CoercionR where
    _ ~== _ = True --eqCoercion might need causion for uniques of type variables 

instance Similar Literal where 
  (LitString l) ~== (LitString l')      = True --l == l' 
  (LitChar c) ~== (LitChar c')          = c == c'  
  (LitNumber ti i) ~== (LitNumber tj j) = ti == tj && i == j 
  (LitFloat r) ~== (LitFloat p)         = r == p  
  (LitDouble r) ~== (LitDouble p)       = r == p 
  l ~== k                               = True 

instance Similar [Alt Var] where 
    xs ~== ys = all (uncurry (~==)) (zip xs ys)
 
instance Similar (Alt Var) where
    (Alt ac vs e) ~== (Alt ac' vs' e') = --trace "ALTVAR" 
        ac ~== ac' && vs ~== vs' && e ~== e'

instance Similar [Var] where 
    xs ~== ys = --trace "[VAR]"
                all (uncurry (~==)) (zip xs ys)

instance Similar AltCon where
    (DataAlt a) ~== (DataAlt a') = a ~== a'  
    (LitAlt l)  ~== (LitAlt l')  = l ~== l -- literals will probably never match
    _           ~== _ = True -- DEFAULT 

instance Similar DataCon where 
    x ~== y = --trace "DATACON" 
        dataConName x == dataConName y 

instance Similar Var where
    v1 ~== v2 = --trace ("VAR " ++ "v1: " ++ showSDocUnsafe (ppr (varName v1)) ++" v2: " ++ showSDocUnsafe (ppr (varName v2))) 
        getOccString v1 == getOccString v2 

instance Similar Type where
    k1 ~== k2 = --trace ("TYPEEQ: " ++ "T1" `sp` (showSDocUnsafe $ ppr k1) `sp` "T2" `sp` (showSDocUnsafe $ ppr k2)) 
               show k1 == show k2  -- to disregard uniques of typevars from different programs (after renaming we might be able to use eqType)
               -- using eqType would require same uniques, which we don't have across different compilations
               -- would require "renaming" all uniques from a large storage of fixed uniques 

-- ==================

instance Eq (Bind Var) where 
    (Rec es) == (Rec xs) = all (\((b,e),(b',e')) -> b `eqVar` b' && e == e') (zip es xs)
    (NonRec v b) == (NonRec i c) = v `eqVar` i && b == c 
    x == y = False 

instance Eq (Expr Var) where 
    (Var id) == (Var id')                  = --trace "ID" 
                                             id `eqVar` id
    (Type t) == (Type t')                  = --trace "TYPE" 
                                             t == t'
    (Lit l)  == (Lit l')                   = --trace "LIT" True --l == l -- No point in checking equality of literals, they will not be equal in most cases
                                             True 
    (App e arg) == (App e' arg')           = --trace "APP" 
                                             e == e' && arg == arg'
    (Lam b e) == (Lam b' e')               = --trace "LAM" 
                                             b `eqVar` b' && e == e' -- check that the type of the head is equal                                      
    (Case e v t as) == (Case e' v' t' as') = --trace "CASE" 
                                             t == t' && e == e' && as == as' 
    (Cast e co) == (Cast e' co')           = --trace "CAST" 
                                             co == co' && e == e'
    (Let b e)   == (Let b' e')             = --trace ("LET1: " ++ show b ++ " in " ++ show e `nl` "LET2: " ++ show b' ++ " in " ++ show e') 
                                             e == e' && b == b' 
    x == y                                 = False 

instance (Eq Type) where 
    t1 == t2 = show t1 == show t2 


eqVar :: Var -> Var -> Bool 
eqVar v1 v2 = getOccString v1 == getOccString v2 

instance Eq CoercionR where
    c == c' = True 
 
instance Eq (Alt Var) where
    (Alt ac vs e) == (Alt ac' vs' e') = ac == ac' && vs == vs' && e == e' --- SEEMS LIKE THIS GETS CALLED BY SOMEONE




