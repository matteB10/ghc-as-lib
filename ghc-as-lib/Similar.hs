{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isJust" #-}
module Similar where


import GHC.Core ( Expr(..), CoreExpr, CoreBndr, Bind(..), CoreProgram, Alt(..), AltCon(..))
import GHC.Unit.Types
import GHC.Types.Var (Var(..), isTyVar, tyVarKind, isTcTyVar, isId)
import GHC.Core.TyCo.Rep (Type(..), Kind, TyLit (StrTyLit), AnonArgFlag (VisArg), Coercion)
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

import Utils ( isHoleVar, sp, isHoleVarExpr, isHoleExpr, isPatError)
import GHC.Core.Coercion (eqCoercion)

class Similar a where
    (~=) :: a -> a -> Bool
    (~>)  :: a -> a -> Bool
    -- (~?) :: a -> a -> Maybe HoleMap

instance Similar CoreProgram where
    (x:xs) ~> (y:ys) = x ~> y && xs ~> ys
    [] ~> []         = True
    _ ~> _           = trace "different number of binders" False

    (x:xs) ~= (y:ys) = x ~= y && xs ~= ys
    [] ~= []         = True
    _ ~= _           = trace "different number of binders" False


--- Without trace -------------------------------------
instance Similar (Bind Var) where
    (Rec es) ~> (Rec es')          = es ~> es' 
    (NonRec v e) ~> (NonRec v' e') = v ~> v' && e ~> e'
    (NonRec v e) ~> Rec [(v',e')]  = v ~> v' && e ~> e'
    _ ~> _                         = False

    (Rec es) ~= (Rec es')          = es ~= es'
    (NonRec v e) ~= (NonRec v' e') = v ~= v' && e ~= e'
    x ~= y = False

instance Similar [(Var,Expr Var)] where
    es ~> es' = all (\((b,e),(b',e')) -> b ~> b' && e ~> e') (zip es es') && length es == length es' 
    es ~= es' = all (\((b,e),(b',e')) -> b ~= b' && e ~= e') (zip es es') && length es == length es' 

instance Similar (Expr Var) where
    (Var id) ~> (Var id')                  = id ~> id'
    (Type t) ~> (Type t')                  = t ~> t'
    (Lit l)  ~> (Lit l')                   = l ~> l'
    (App (App f e) a) ~> (App (App f' e') a') | isCommutative f
                                               , f ~> f' = (e ~> e' && a ~> a') || e ~> a' && e' ~> a
    (App e arg) ~> (App e' arg')           = e ~> e' && arg ~> arg'

    (Lam b e) ~> (Lam b' e')               = b  ~> b' && e ~> e' -- check that the type of the head is equal                                      
    (Case e v t as) ~> (Case e' v' t' as') = t  ~> t' && e ~> e' && as ~> as'
    (Cast e co)  ~> (Cast e' co')          = co ~> co' && e ~> e'
    (Let b e)    ~> (Let b' e')            = b  ~> b' && e ~> e'
    (Coercion c) ~> (Coercion c')          = c  ~> c'
    x ~> y                                 = isHoleVarExpr x || isHoleVarExpr y

    (Var id) ~= (Var id')                  = id ~= id'
    (Type t) ~= (Type t')                  = t ~= t'
    (Lit l)  ~= (Lit l')                   = l ~= l'
    (App (App f e) a) ~= (App (App f' e') a') | isCommutative f
                                               , f ~= f' = (e ~= e' && a ~= a') || e ~= a' && e' ~= a
    (App e arg) ~= (App e' arg')           = e ~= e' && arg ~= arg'

    (Lam b e) ~= (Lam b' e')               = b  ~= b' && e ~= e' -- check that the type of the head is equal                                      
    (Case e v t as) ~= (Case e' v' t' as') = t  ~= t' && e ~= e' && as ~= as'
    (Cast e co)  ~= (Cast e' co')          = co ~= co' && e ~= e'
    (Let b e)    ~= (Let b' e')            = b  ~= b' && e ~= e'
    (Coercion c) ~= (Coercion c')          = c  ~= c'
    x ~= y                                 = False



instance Similar Coercion where
    c1 ~> c2 = True -- eqCoercion c1 c2 -- uniques of type vars might be a problem?  
    c1 ~= c2 = True


instance Similar Literal where
  (LitString l)    ~> (LitString l')   = True -- accept all litstrings instead of replacing lit strings 
  (LitChar c)      ~> (LitChar c')     = c == c'
  (LitNumber ti i) ~> (LitNumber tj j) = ti == tj && i == j
  (LitFloat f)     ~> (LitFloat f')    = f == f'
  (LitDouble d)    ~> (LitDouble d')   = d == d'
  l ~> k                               = True

  (~=) = (~>)

instance Similar [Alt Var] where
    xs ~> ys = all (uncurry (~>)) (zip xs ys)
    xs ~= ys =  all (uncurry (~=)) (zip xs ys)

instance Similar (Alt Var) where
    (Alt ac vs e) ~> (Alt ac' vs' e') | --trace ("isPatErr" ++ show (isPatError e) ++ show e) 
                                        isPatError e = ac ~> ac' && vs ~> vs' -- if pattern error, student has missing cases, we dont check nested cases
                                      | otherwise     = ac ~> ac' && vs ~> vs' && e ~> e'
    (Alt ac vs e) ~= (Alt ac' vs' e')  = ac ~= ac' && vs ~= vs' && e ~= e'

instance Similar [Var] where
    xs ~> ys = length xs == length ys &&
        all (uncurry (~>)) (zip xs ys)
    xs ~= ys = length xs == length ys &&
        all (uncurry (~>)) (zip xs ys)

instance Similar AltCon where
    (DataAlt a) ~> (DataAlt a') = a ~> a'
    (LitAlt l)  ~> (LitAlt l')  = l ~> l'
    DEFAULT     ~> DEFAULT      = True
    _ ~> _                      = False
    (~=) = (~>)

instance Similar DataCon where
    x ~> y = dataConName x == dataConName y
    (~=) = (~>)

instance Similar Var where
    v1 ~> v2 = (isHoleVar v1 || isHoleVar v2) || getOccString v1 == getOccString v2
    v1 ~= v2 = getOccString v1 == getOccString v2

instance Similar Type where
    k1 ~> k2 = show k1 == show k2
    (~=) = (~>)
            -- to disregard uniques of typevars from different programs we don't use eqType
            -- using eqType would require same uniques, which we don't have across different compilations
            -- would require "renaming" all uniques from a large storage of fixed uniques 

-- ==================
-- Needed for list deletion
instance Eq (Bind Var) where
    (==) = (~>)

instance Eq (Expr Var) where
    (==) = (~>)

{- we need checks for e.g  xs == reverse xs ~== reverse xs == xs 
however, pattern matching like below does not scale well at all. 
Need a clever approach. 
(App f@(App op e1) e2) ~== (App f'@(App op' e1') e2') 
                                                | isCommutative op =  trace ("op1" ++ show op ++ " op2:" ++ show op') $ 
                                                                op ~== op' && e1 ~== e1' || e2 ~== e2' || e1 ~== e2' && e2 ~== e1' 
                                                | otherwise = f ~== f' && e2 ~== e2' 
-}
isCommutative :: CoreExpr -> Bool
-- | Check if we are applying something commutative, then the order of the arguments are irrelevant
-- not sure how to check this, functions are just variables
isCommutative (Var op) = getOccString op `elem` ["==", "/=", "+", "*", "&&", "||"] -- hardcode common ones 
isCommutative _ = False