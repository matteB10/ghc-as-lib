{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isJust" #-}
module Diff where


import GHC.Core ( Expr(..), CoreExpr, CoreBndr, Bind(..), CoreProgram, Alt(..), AltCon(..))
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
import Data.Generics.Uniplate.Data ( children )
import GHC.Core.Stats (exprSize)


class Diff a where
    (~~) :: a -> a -> Int

instance Diff CoreProgram where
    (x:xs) ~~ (y:ys) = x ~~ y + xs ~~ ys
    [] ~~ []         = 0
    _ ~~ _           = trace "different number of binders" 100 

--- Without trace -------------------------------------
instance Diff (Bind Var) where
    (Rec es) ~~ (Rec es') = sum $ zipWith (curry (\((b,e),(b',e')) -> b ~~ b' + e ~~ e')) es es'
    (NonRec v e) ~~ (NonRec v' e') = v ~~ v' + e ~~ e'
    x ~~ y  = case (x,y) of 
        (NonRec v e,Rec ((v',e'):es))  -> v ~~ v' + e ~~ e' + length es 
        (Rec ((v',e'):es),NonRec v e)  -> v ~~ v' + e ~~ e' + length es 


instance Diff (Expr Var) where
    (Var id) ~~ (Var id')                  = id ~~ id'
    (Type t) ~~ (Type t')                  = t ~~ t'
    (Lit l)  ~~ (Lit l')                   = l ~~ l' 
    (App e arg) ~~ (App e' arg')           = e ~~ e' + arg ~~ arg'
    (Lam b e) ~~ (Lam b' e')               = b ~~ b' + e ~~ e'                                      
    (Case e v t as) ~~ (Case e' v' t' as') = t ~~ t' + e ~~ e' + as ~~ as'
    (Cast e co) ~~ (Cast e' co')           = co ~~ co' + e ~~ e'
    (Let b e)   ~~ (Let b' e')             = b ~~ b' + e ~~ e'
    (Coercion c) ~~ (Coercion c')          = c ~~ c'
    x ~~ y                                 | isHoleExpr x || isHoleExpr y = 0 
                                           | otherwise = --trace (show x ++ " AGAINST " ++ show y) $
                                                         1 + getDiff xc yc 
        where xc = children x 
              yc = children y 
              getDiff (s:ss) (m:ms) = s ~~ m + getDiff ss ms  
              getDiff [] []         = 0
              getDiff [] xs         = sum (map exprSize xs)
              getDiff ys  []        = sum (map exprSize ys)

instance Diff CoercionR where
    _ ~~ _ = 0 --eqCoercion might need causion for uniques of type variables 

instance Diff Literal where
  (LitString l) ~~ (LitString l')      = 0
  (LitChar c) ~~ (LitChar c')          = if c == c' then 0 else 1
  (LitNumber ti i) ~~ (LitNumber tj j) = if ti == tj && i == j then 0 else 1
  (LitFloat r) ~~ (LitFloat p)         = if r == p then 0 else 1
  (LitDouble r) ~~ (LitDouble p)       = if r == p then 0 else 1
  l ~~ k                               = 1
instance Diff [Alt Var] where
    xs ~~ ys = sum $ zipWith (~~) xs ys

instance Diff (Alt Var) where
    (Alt ac vs e) ~~ (Alt ac' vs' e') = ac ~~ ac' + vs ~~ vs' + e ~~ e'

instance Diff [Var] where
    xs ~~ ys = sum $ zipWith (~~) xs ys

instance Diff AltCon where
    (DataAlt a) ~~ (DataAlt a') = a ~~ a'
    (LitAlt l)  ~~ (LitAlt l')  = l ~~ l 
    DEFAULT     ~~ DEFAULT      = 0
    _ ~~ _                      = 1

instance Diff DataCon where
    x ~~ y | dataConName x == dataConName y = 0
           | otherwise = 1

instance Diff Var where
    v1 ~~ v2 | getOccString v1 == getOccString v2  || isHoleVar v1 || isHoleVar v2 = 0
             | otherwise = 1

instance Diff Type where
    k1 ~~ k2 | show k1 == show k2 = 0
             | otherwise = 1


