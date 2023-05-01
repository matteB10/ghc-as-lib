{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
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
      Literal (..),
      fsLit,
      showPpr,
      getOccString,
      tyVarKind, showSDoc, showSDocUnsafe, isTyVar ) 
import GHC.Core.TyCo.Rep
    ( TyLit(StrTyLit),
      Type(TyConApp, LitTy, AppTy, FunTy, CastTy, ft_af, ft_arg, ft_res,
           ft_mult) )
import GHC.Utils.Encoding (utf8DecodeByteString)

-- General imports 
import Data.Maybe ( isNothing, fromJust )
import Data.Generics.Uniplate.Data  
import Control.Monad (when)

-- Local imports 
import Instance 
import GHC.Utils.Outputable
import Language.Haskell.TH.Lib (conT)
import GHC.RTS.Flags (getParFlags)
import GHC.Core (CoreExpr, CoreBind)
import GHC.Core.Predicate (isEvVar)
import Data.Data ( Data )

type ExerciseName = String 


showGhc :: (Outputable a) => DynFlags -> a -> String
-- | Pretty print ghc stuff 
showGhc = showPpr 

showGhcUnsafe :: (Outputable a) => a -> String  
showGhcUnsafe = showSDocUnsafe . ppr 

printGhc :: (Outputable a) => a -> IO ()
printGhc = putStrLn . showGhcUnsafe

banner :: [Char] -> IO ()
banner msg = putStrLn $ "\n\n--- " ++ msg ++ " ---\n\n"

-- String concatenation helpers
sp, nl, cm :: String -> String -> String
sp x y = x ++ " " ++ y
nl x y = x ++ "\n" ++ y
cm x y = x ++ " , " ++ y

getExerciseName :: FilePath -> ExerciseName 
-- very hard coded, works for current file structure
getExerciseName fp = takeWhile (/= '/') $ drop 1 $ dropWhile (/= '/') fp 


varNameUnique :: Var -> String 
varNameUnique = showSDocUnsafe . ppr 


isCaseExpr :: CoreExpr -> Bool 
isCaseExpr (Case {}) = True 
isCaseExpr _         = False 

isHoleExpr :: CoreExpr-> Bool
isHoleExpr (Case e _ t _) = case getTypErr e of 
                                Just pe -> True 
                                _ -> False 
isHoleExpr _              = False

isPatError :: CoreExpr-> Bool 
isPatError (Case e _ t _) = case getPatErr e of 
                                Just pe -> True 
                                _ -> False 
isPatError _              = False


getHoleMsg :: CoreExpr -> String 
getHoleMsg e = concat [getLitString l | str@(Lit l) <- universe e, isTypedHolErrMsg l]

isHoleVar :: Var -> Bool
isHoleVar v = take 4 (getOccString v) == "hole"

isHoleVarExpr :: CoreExpr-> Bool
isHoleVarExpr (Var v) = isHoleVar v 
isHoleVarExpr _ = False 

isEvOrTyVar :: Var -> Bool 
isEvOrTyVar v = isTyVar v || isEvVar v 

isEvOrTyExp :: CoreExpr -> Bool 
isEvOrTyExp e = case e of   
    (Var v)  -> isEvOrTyVar v 
    (Type t) -> True
    _        -> False 

isTy :: CoreExpr -> Bool
isTy (Type _) = True 
isTy (Var v)  = isTyVar v 
isTy _        = False 

ins :: Data (Bind Var) => Var -> CoreExpr -> Bool
-- | Find if a variable is used somewhere in an expression
ins v e = or [v==v' | v' <- universeBi e :: [Var]]

insB :: Data (Bind Var) => Var -> CoreBind -> Bool
-- | Find if a variable is used somewhere in a binder
insB n b = or [v==n | v <- universeBi b :: [Var]]

subst :: Var -> Var -> CoreExpr -> CoreExpr
subst v v' = --trace ("found subst" ++ show "["++ show v' ++ "->" ++ show v ++"]" ) $
             transformBi (sub v v')

sub :: Var -> Var -> CoreExpr -> CoreExpr
-- | Replace the second variable with the first one given
sub v v' = \case
    (Var id) | id == v' -> (Var v)
    e -> e

subE :: CoreExpr -> Var -> CoreExpr -> CoreExpr
-- | Replace the variable with an expression 
subE e v = transformBi $ \case
    (Var id) | id == v -> e 
    e -> e

getAltExp :: Alt Var -> CoreExpr 
getAltExp (Alt _ _ e) = e 

getLitString :: Literal -> String 
getLitString (LitString l) = utf8DecodeByteString l 

getTypErr :: CoreExpr-> Maybe Var
getTypErr = getVarFromName "typeError" 

getPatErr :: CoreExpr-> Maybe Var 
getPatErr = getVarFromName "patError"

getTypErrB :: CoreBind -> Maybe Var
getTypErrB (NonRec _ e) = getTypErr e
getTypErrB (Rec es) = case filter isNothing errs of
                    [] -> Nothing
                    l  -> Just (fromJust $ head l)
        where errs = map (getTypErr . snd) es


getVarFromName :: String -> CoreExpr-> Maybe Var
getVarFromName name e | null vars = Nothing 
                      | otherwise = head vars -- just return first found variable if any matching  
    where vars = [Just v | (Var v) <- universe e, getOccString v == name]

{- containsErr :: String -> Expr Var -> Maybe (Expr Var)
containsErr err = transformBiM $ \ex -> case ex :: Expr Var of 
    e@(Var id) | isErrVar err id -> Just e 
               | otherwise       -> Nothing 
    _ -> Nothing  

containsTErr :: String -> Expr Var -> [Maybe (Expr Var)]
containsTErr err (Var id)   | isErrVar err id = [Just $ Var id]
                            | otherwise = [Nothing]
containsTErr _ (Lit l)        = [Nothing]
containsTErr err (App e arg)    = containsTErr err e ++ containsTErr err arg
containsTErr err (Lam b e)      = containsTErr err e
containsTErr err (Case e _ _ _) = containsTErr err e
containsTErr err (Cast e co)    = containsTErr err e
containsTErr _ _              = [Nothing] -}


isVar :: CoreExpr-> Bool
isVar (Var _) = True
isVar _       = False

isErrVar :: String -> Var -> Bool
isErrVar s v = getOccString v == s 

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