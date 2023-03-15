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
import Instance
import GHC.Core.TyCon (TyCon)
import Control.Monad (when)
import Data.Maybe (isNothing)
import GHC.Core.DataCon (DataCon(..), dataConName)
import GHC.Utils.Outputable (showSDocUnsafe, Outputable (ppr))
import GHC.Types.Literal (Literal(..), LitNumType)

import Utils 

class Similar a where
    (~==) :: a -> a -> Bool

instance Similar CoreProgram where
    (x:xs) ~== (y:ys) = x ~== y && xs ~== ys
    [] ~== []         = True
    _ ~== _           = trace "different number of binders" False 

--- Without trace -------------------------------------
instance Similar (Bind Var) where
    (Rec es) ~== (Rec es')          = and $ concatMap (\(x,x') -> map (\(y,y') -> x ~== y && x' ~== y') es) es'
    (NonRec v e) ~== (NonRec v' e') = v ~== v' && e ~== e'
    x ~== y                         = case x of -- TODO: How should I treat recs/nonrecs? can occur because of holes 
                        (Rec ((v,e):vs)) -> let NonRec v' e' = y   in v ~== v' && e ~== e'
                        (NonRec v' e')   -> let Rec ((v,e):vs) = y in v ~== v' && e ~== e'


instance Similar (Expr Var) where
    (Var id) ~== (Var id')                  = --trace "ID" 
                                              id ~== id
    (Type t) ~== (Type t')                  = --trace "TYPE" 
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
    x ~== y                                 | isHoleExpr x || isHoleExpr y = --trace ("isHole: type1:" ++ (showSDocUnsafe $ ppr (typeE x)) `nl` "type2:" `sp` (showSDocUnsafe $ ppr (typeE y))) 
                                                                            True -- before alpharenaming/case expr holes not replaced 
                                            | isHole' x || isHole' y = --trace ("isHole:" ++ "x: " ++ show x ++ "y: " ++ show y) 
                                                                       True -- if hole replaced with hole variable
                                            | otherwise = --trace ("OTHER: X:" `sp` show x `sp` " Y: " `sp` show y)
                                                          False

instance Similar CoercionR where
    c ~== c' = True -- how should this be checked?

instance Similar Literal where 
  --(LitString l) ~== li                  = True 
  --(LitChar c) ~== li                    = True 
  (LitNumber ti i) ~== (LitNumber tj j) = ti == tj && i == j 
  (LitFloat r) ~== (LitFloat p)         = r == p  
  (LitDouble r) ~== (LitDouble p)       = r == p 
  l ~== k                               = True 

r :: Rational 
r = undefined 

instance Similar [Alt Var] where 
    xs ~== ys = all (uncurry (~==)) (zip xs ys)
 
instance Similar (Alt Var) where
    (Alt ac vs e) ~== (Alt ac' vs' e') = --trace "ALTVAR" 
        ac ~== ac' && vs ~== vs' && e ~== e'

instance Similar [Var] where 
    xs ~== ys = all (uncurry (~==)) (zip xs ys)

instance Similar AltCon where
    (DataAlt a) ~== (DataAlt a') = a ~== a'  
    (LitAlt l)  ~== (LitAlt l')  = True -- literals will probably never match
    _           ~== _ = True -- DEFAULT 

instance Similar DataCon where 
    x ~== y = --trace "DATACON" 
        dataConName x == dataConName y 

instance Similar Var where
    v1 ~== v2 = --trace ("VAR " ++ "v1: " ++ showSDocUnsafe (ppr (varName v1)) ++" v2: " ++ showSDocUnsafe (ppr (varName v2))) 
        getOccString v1 == getOccString v2 

instance Similar Type where
    k1 ~== k2 = --trace ("TYPEEQ: " ++ "T1" `nl` (showSDocUnsafe $ ppr k1) `nl` "T2" `nl` (showSDocUnsafe $ ppr k2)) 
                    show k1 == show k2  -- to disregard uniques of typevars from different programs (after renaming we might be able to use eqType)
        --trace ("type1:" ++ show k1 `nl` "type2:" ++ show k2) show k1 == show k2 ----ugly hack with show, eqType seems to give error in some cases where type is similar 

 ------------------------------------

{- Core Expr 
data Expr b
  = Var   Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]   -- See Note [Case expression invariants]
                                    -- and Note [Why does Case have a 'Type' field?]
  | Cast  (Expr b) CoercionR        -- The Coercion has Representational role
  | Tick  CoreTickish (Expr b)
  | Type  Type
  | Coercion Coercion
  deriving Data
-}


-- =======================================================================
-- 
instance Eq (Bind Var) where 
    (Rec es) == (Rec xs) = and $ concatMap (\(x,x') -> map (\(y,y') -> x `eqVar` y && x' == y') es) xs 
    (NonRec v b) == (NonRec i c) = v `eqVar` i && b == c 
    x == y = False 

instance Eq (Expr Var) where 
    (Var id) == (Var id')                  = trace "ID" id `eqVar` id
    (Type t) == (Type t')                  = trace "TYPE" t == t'
    (Lit l)  == (Lit l')                   = trace "LIT" True --l == l -- No point in checking equality of literals, they will not be equal in most cases
    (App e arg) == (App e' arg')           = trace "APP" e == e' && arg == arg'
    (Lam b e) == (Lam b' e')               = trace "LAM" b `eqVar` b' && e == e' -- check that the type of the head is equal                                      
    (Case e v t as) == (Case e' v' t' as') = trace "CASE" t == t' && e == e' && as == as' 
    (Cast e co) == (Cast e' co')           = trace "CAST" co == co' && e == e'
    (Let b e)   == (Let b' e')             = trace ("LET1: " ++ show b ++ " in " ++ show e `nl` "LET2: " ++ show b' ++ " in " ++ show e') 
                                                    e == e' -- && b == b' 
    x == y                                 = False 

instance (Eq Type) where 
    t1 == t2 = show t1 == show t2 


eqVar :: Var -> Var -> Bool 
eqVar v1 v2 = getOccString v1 == getOccString v2 

instance Eq CoercionR where
    c == c' = True 
 
instance Eq (Alt Var) where
    (Alt ac vs e) == (Alt ac' vs' e') = trace "ALTVAR" ac == ac' && vs == vs' && e == e'




