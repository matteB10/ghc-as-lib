module Utils where 


-- GHC imports 
import GHC
import GHC.Plugins 
import GHC.Core.TyCo.Rep

-- General imports 
import Data.Maybe

-- Local imports 
import Instance 

showGhc :: (Outputable a) => DynFlags -> a -> String
-- | Pretty print ghc stuff 
showGhc = showPpr 

typeE :: Expr Var -> Type
-- | Experimental function returning the type of a Core expr
typeE (Var id)       = varType id
typeE (Type t)       = t
typeE (Lit l)        = LitTy $ StrTyLit (fsLit (show l))
typeE (App e arg)    = case isTyCon (typeE e) of
                        Just t -> TyConApp t [typeE arg]
                        Nothing -> AppTy (typeE e) (typeE arg)
typeE (Lam b e)      = FunTy {ft_af =  VisArg, ft_arg = tyVarKind b, ft_res = typeE e, ft_mult = typeE e}
typeE (Case _ _ t _) = t
typeE (Let b e)      = typeE e
typeE (Cast e co)    = CastTy (typeE e) co

isTyCon :: Type -> Maybe TyCon
isTyCon (TyConApp t _ ) = Just t
isTyCon _               = Nothing

isHoleExpr :: Expr Var -> Bool
isHoleExpr (Case e _ t _) = not (all isNothing (containsTErr e))
isHoleExpr _              = False

isHole' :: Expr Var -> Bool
isHole' (Var v) = take 4 (getOccString v) == "hole"
isHole' _       = False 

containsTErr :: Expr Var -> [Maybe (Expr Var)]
containsTErr (Var id)       | isVarTErr id = [Just $ Var id]
                            | otherwise = [Nothing]
containsTErr (Lit l)        = [Nothing]
containsTErr (App e arg)    = containsTErr e ++ containsTErr arg
containsTErr (Lam b e)      = containsTErr e
containsTErr (Case e _ _ _) = containsTErr e
containsTErr (Cast e co)    = containsTErr e
containsTErr _              = [Nothing]


isVar :: Expr Var -> Bool
isVar (Var _) = True
isVar _       = False

isVarTErr :: Var -> Bool
isVarTErr v = getOccString v == "typeError"

isVarMod :: Var -> Bool
isVarMod v = "$trModule" == take 9 (getOccString v)

-- Concatenation helpers
sp, nl, cm :: String -> String -> String
sp x y = x ++ " " ++ y
nl x y = x ++ "\n" ++ y
cm x y = x ++ " , " ++ y