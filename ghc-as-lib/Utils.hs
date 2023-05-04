{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Utils where 


-- GHC imports 
import GHC ( DynFlags, TyCon, Type, SrcSpan, Name, Id )
import GHC.Plugins
    ( Alt(Alt),
      AnonArgFlag(VisArg),
      Bind(..),
      Expr(..),
      Literal(LitString),
      Var(..),
      Outputable,
      CoreBndr,
      CoreProgram,
      Literal (..),
      fsLit,
      showPpr,
      getOccString,
      tyVarKind, showSDoc, showSDocUnsafe, isTyVar, Unique, mkGlobalVar, mkInternalName, mkOccName, mkGeneralSrcSpan, mkFastString, vanillaIdInfo, UniqSupply, uniqFromSupply, setIdNotExported, setVarName, mkLocalId ) 
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
import GHC.Core (CoreExpr, CoreBind, isId)
import GHC.Core.Predicate (isEvVar)
import Data.Data ( Data )
import qualified GHC.Types.Name.Occurrence as Occ
import GHC.Types.Id.Info (IdDetails(..))

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


fresh :: UniqSupply -> Var -> Var 
fresh us id = 
    let uq = uniqFromSupply us
        name = makeName "fresh" uq (mkGeneralSrcSpan (mkFastString "Dummy location"))
        id'  = setIdNotExported $ makeLocal $ setVarName id name -- reuse id information from top-level binder
    in id'


makeName :: String -> Unique -> SrcSpan -> Name
-- | Create a name from a string and a variable
--   used for renaming variables
makeName n uq loc = mkInternalName uq (mkOccName Occ.varName n) loc

makeLocal :: Var -> Var
makeLocal v | isId v = mkLocalId (varName v) (varMult v) (varType v)


makeGlobVar :: Unique -> Type -> String -> Var 
makeGlobVar uq t n = mkGlobalVar id_det name t id_inf
        where id_det = VanillaId
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              id_inf = vanillaIdInfo

varNameUnique :: Var -> String 
varNameUnique = showSDocUnsafe . ppr

updateVar :: Var -> CoreBind -> CoreBind
-- | update variable information 
updateVar v = transformBi $ \e -> case e :: CoreExpr of
        (Var v') | v == v' -> Var v
        e       -> e

getBindTopVar :: CoreBind -> Var
-- | Get variable of a binder 
getBindTopVar (NonRec v _) = v
getBindTopVar (Rec ((v,e):_)) = v


getBinds :: [CoreBind] -> Var -> [CoreBind]
-- | Get all binders containing a certain variable
getBinds binds v = [r | r <- binds, v `insB` r]


isCaseExpr :: CoreExpr -> Bool 
isCaseExpr (Case {}) = True 
isCaseExpr _         = False 

isHoleExpr :: CoreExpr -> Bool
-- | Check if a case expression is a typed hole expression
isHoleExpr (Case e _ t _) = case getTypErr e of 
                                Just pe -> True  -- need to check hasHoleMsg
                                _ -> False       -- if deferring all type errors
isHoleExpr _              = False                -- and not only typed holes

isPatError :: CoreExpr -> Bool 
isPatError (Case e _ t _) = case getPatErr e of 
                                Just pe -> True 
                                _ -> False 
isPatError _              = False

isPatErrVar :: CoreExpr -> Bool 
isPatErrVar (Var v) = isErrVar "patError" v 
isPatErrVar _       = False


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
-- | Is type or type/evidence variable
isEvOrTyExp e = case e of   
    (Var v)  -> isEvOrTyVar v 
    (Type t) -> True
    _        -> False 

isTyOrTyVar :: CoreExpr -> Bool
isTyOrTyVar (Type _) = True 
isTyOrTyVar (Var v)  = isTyVar v 
isTyOrTyVar _        = False 

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


isVar :: CoreExpr-> Bool
isVar (Var _) = True
isVar _       = False

isErrVar :: String -> Var -> Bool
isErrVar s v = getOccString v == s 

isVarMod :: Var -> Bool
isVarMod v = "$trModule" == take 9 (getOccString v)

hasHoleMsg :: CoreExpr -> Bool 
hasHoleMsg e = not $ null [l | Lit l <- universe e, isTypedHolErrMsg l]

isTypedHolErrMsg :: Literal -> Bool
isTypedHolErrMsg (LitString l) = let msg = lines $ utf8DecodeByteString l
                                 in "hole" `elem` (words (msg !! 1))
isTypedHolErrMsg _ = False

