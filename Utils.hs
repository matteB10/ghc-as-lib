module Utils where 


-- GHC imports 
import GHC ( DynFlags, TyCon, Type )
import GHC.Plugins
    ( Alt(Alt),
      AnonArgFlag(VisArg),
      Bind(..),
      Expr(..),
      Literal(LitString),
      Var(varName, varType),
      Outputable,
      CoreBndr,
      CoreProgram,
      fsLit,
      showPpr,
      getOccString,
      tyVarKind, showSDoc, showSDocUnsafe ) 
import GHC.Core.TyCo.Rep
    ( TyLit(StrTyLit),
      Type(TyConApp, LitTy, AppTy, FunTy, CastTy, ft_af, ft_arg, ft_res,
           ft_mult) )
import GHC.Utils.Encoding (utf8DecodeByteString)

-- General imports 
import Data.Maybe ( isNothing )
import Data.Generics.Uniplate.Data  
import Control.Monad (when)

-- Local imports 
import Instance 
import GHC.Utils.Outputable

type ExerciseName = String 


showGhc :: (Outputable a) => DynFlags -> a -> String
-- | Pretty print ghc stuff 
showGhc = showPpr 

banner :: [Char] -> IO ()
banner msg = putStrLn $ "\n\n--- " ++ msg ++ " ---\n\n"

-- String concatenation helpers
sp, nl, cm :: String -> String -> String
sp x y = x ++ " " ++ y
nl x y = x ++ "\n" ++ y
cm x y = x ++ " , " ++ y


varNameUnique :: Var -> String 
varNameUnique = showSDocUnsafe . ppr 

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

findLiterals :: CoreProgram -> String
findLiterals cs = concat (concatMap findLits cs)

findLits :: Bind CoreBndr -> [String]
findLits (NonRec b exps)  = findLit exps
findLits (Rec es)         = concatMap (findLit . snd) es

findLit :: Expr CoreBndr -> [String]
findLit (Var id)     = []
findLit (Lit l)      = [show l] -- ++ "\n"-- here is type errors stored, print them
findLit (App e bndr) = findLit e ++ findLit bndr
findLit (Lam b e)    = findLit e
findLit (Let bi e)   = findLits bi ++ findLit e
findLit (Case e _ _ alts) = findLit e ++ concatMap (\(Alt _ _ e) -> findLit e) alts --Alt AltCon [b] (Expr b)
findLit (Cast  e _)  = findLit e
findLit (Tick _ e)   = findLit e
findLit (Type t)     = []
findLit (Coercion c) = []

printHoleLoc :: CoreProgram -> IO ()
printHoleLoc = mapM_ printHoleLoc'

printHoleLoc' :: Bind CoreBndr -> IO ()
printHoleLoc' (NonRec _ exps)  = printHoleLc exps
printHoleLoc' (Rec es)         = mapM_ (printHoleLc . snd) es

printHoleLc :: Expr CoreBndr -> IO ()
printHoleLc (Var var) | getOccString (varName var) == "typeError" = return ()
                   | otherwise =  return () --putStrLn ("occname: " ++ getOccString name ++ " , unique: " ++ show (nameUnique name))
                        where name = varName var
printHoleLc (Lit l)            = when (isTypedHolErrMsg l) $ let ((r,c), t) = holeTypeFromMsg (show l)
                                                          in putStrLn $ "found hole at " ++ p (r ++ ":" ++ c) ++ " with type: " ++ t
printHoleLc (App e bndr)       = printHoleLc e >> printHoleLc bndr
printHoleLc (Lam b e)          = printHoleLc e
printHoleLc (Let bi e)         = printHoleLoc' bi >> printHoleLc e
printHoleLc (Case e _ _ alts)  = printHoleLc e >> mapM_ (\(Alt _ _ e) -> printHoleLc e) alts --alts :: Alt AltCon [b] (Expr b)
printHoleLc (Cast  e _)        = printHoleLc e
printHoleLc (Tick _ e)         = printHoleLc e
printHoleLc (Type t)           = return ()
printHoleLc (Coercion c)       = return ()


isTypedHolErrMsg :: Literal -> Bool
isTypedHolErrMsg (LitString l) = let msg = lines $ utf8DecodeByteString l
                                 in last msg == "(deferred type error)"
isTypedHolErrMsg _ = False


holeTypeFromMsg :: String -> ((String, String), String)
holeTypeFromMsg s = ((row,col), htype) --trace (show (lines s) ++ show (row,col,htype)) 
    where (l1:l2:ls) = if length (lines s) > 1 then lines s else ["no hole", "is found"]
          rm    =  drop 1 (dropWhile (/= ':') l1)
          row   = takeWhile (/= ':') rm
          col   = takeWhile (/= ':') (drop (length row + 1) rm)
          hname = takeWhile (/= ' ') (drop 2 (dropWhile (/= ':') l2)) -- if hole suffixed with identifier
          htype = drop 3 $ dropWhile (/= ':') (drop 1 (dropWhile (/= ':') l2))