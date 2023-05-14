{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Utils where


-- GHC imports 
import GHC ( DynFlags, TyCon, Type, SrcSpan, Name, Id, RealSrcLoc, ParsedSource, LHsExpr, HsDecl (..), GhcPs, HsModule (..), getLocA, Located, SrcSpanAnnA, GRHSs (..), GRHS (GRHS), HsExpr (HsVar), LHsDecl, unLoc, Sig (..) )
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
import Data.Maybe ( isNothing, fromJust, catMaybes, isJust )
import Data.Generics.Uniplate.Data
import Control.Monad (when)

-- Local imports 
import Instance
import GHC.Utils.Outputable
import GHC.Core (CoreExpr, CoreBind, isId)
import GHC.Core.Predicate (isEvVar)
import Data.Data ( Data )
import qualified GHC.Types.Name.Occurrence as Occ
import GHC.Types.Id.Info (IdDetails(..))
import GHC (GenLocated(..), RealSrcSpan, ParsedModule, Ghc)
import GHC.Hs (HsMatchContext(..))
import GHC.Types.SrcLoc (srcSpanToRealSrcSpan)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy (splitOn)
import Debug.Trace (trace)

type ExerciseName = String
type ExercisePath = String 


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

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip [] = []
lstrip (x:xs) | x `elem` chars = lstrip xs
              | otherwise      = x:xs

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

chars :: String
chars = " \t\r\n"

hasTypSig :: String -> ParsedSource -> Bool
-- | Check if a type signature is explicitely declared for 
-- the main function of the exercise 
hasTypSig s ps = s `elem` concatMap (words . showGhcUnsafe) (getSigs ps)

getSig :: String -> ParsedSource -> Sig GhcPs 
-- | Get type signature matching given function name
getSig n ps = head $ map (fromJust . get_type_sig) (getSigs ps) 
    where get_type_sig sig = case sig of
            ts@(TypeSig _ [name] _) | trace (showGhcUnsafe name ++ ":" ++ n) showGhcUnsafe name == n -> Just ts 
            _ -> Nothing 

getDecls :: ParsedSource -> [LHsDecl GhcPs]
getDecls (L l hsm) = hsmodDecls hsm

getSigs :: ParsedSource -> [Sig GhcPs]
getSigs ps = map ((\(SigD nf x) -> x) . unLoc) $ filter isSigD (getDecls ps)
    where isSigD = \case
             (L _ (SigD _ _)) -> True
             _                -> False


mainTypeSigMatches :: String -> ParsedSource -> ParsedSource -> Bool
-- | An (ugly) comparison checking if type signature matches model solution 
mainTypeSigMatches fun sps mps = nonEmpty studfuns && (head studfuns == head modFuns)
    where studfuns = filter ((== fun) . fst) (map splitSig $ getSigs sps)
          modFuns  = filter ((== fun) . fst) (map splitSig $ getSigs mps)
          splitSig s = let funname = strip $ takeWhile (/= ':') (showGhcUnsafe s)
                           funtype = strip $ drop (length funname + 2) (showGhcUnsafe s)
                        in (funname,funtype)

nonEmpty :: [a] -> Bool
nonEmpty = not . null

getHsExprFromLoc :: RealSrcSpan -> ParsedSource -> Maybe [LHsExpr GhcPs]
getHsExprFromLoc rss ps | nonEmpty locExps = return locExps
                        | otherwise        = Nothing
    where exps     = universeBi ps :: [LHsExpr GhcPs]
          locExps  = catMaybes $ filter isJust $ map (matchRealSpan rss) exps


getHsRhsFromLoc :: RealSrcSpan -> ParsedSource -> Maybe [LHsExpr GhcPs]
getHsRhsFromLoc rss ps@(L l hsm) | nonEmpty locExDec = return locExDec
                                 | otherwise         = Nothing
    where locDecls = catMaybes $ filter isJust $ map (matchRealSpan rss) (hsmodDecls hsm)
          locExDec = [c | ex@(L loc (GRHS a b c)) <- universeBi locDecls :: [GenLocated SrcSpan (GRHS GhcPs (LHsExpr GhcPs))]]


matchRealSpan :: RealSrcSpan -> GenLocated SrcSpanAnnA a -> Maybe (GenLocated SrcSpanAnnA a)
matchRealSpan rss ex = case srcSpanToRealSrcSpan (getLocA ex) of
              (Just realspan) | realspan == rss -> Just ex
              _                                 -> Nothing

mapVar2Str :: Map Var Var -> Map String String
-- | Convert a map of variables to a map with their occurence names 
mapVar2Str m = Map.mapKeys getOccString (Map.map getOccString m)

translateNames :: Map Var Var -> Map Var Var -> Map String String
-- | Create a translation map from model variable names to student variable names 
translateNames studMap modMap = Map.fromList (map go modList)
    where modList  = Map.toList (mapVar2Str modMap)
          studMapFlipped = keysToVals (mapVar2Str studMap)
          go :: (String, String) -> (String, String)
          go (m_src,m_new) = case Map.lookup m_new studMapFlipped of
            Just s_src -> (m_src, s_src)
            Nothing -> (m_src,"unknown") -- should preferably never happen 

keysToVals :: Map String String -> Map String String
keysToVals mp = Map.fromList (zip vals keys)
   where keys = Map.keys mp
         vals = Map.elems mp

substHs :: Map String String -> LHsExpr GhcPs -> LHsExpr GhcPs
substHs names = rewriteBi $ \e -> case e :: LHsExpr GhcPs of
        L l (HsVar a b) -> return $ L l (HsVar a b) -- this function does absolutely nothing right now
        ex              -> return ex                -- must figure out how to rename HsVars 


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
isCaseExpr (Case {})  = True
isCaseExpr (Tick _ e) = isCaseExpr e
isCaseExpr _          = False

isHoleExpr :: CoreExpr -> Bool
-- | Check if a case expression is a typed hole expression
isHoleExpr (Case e _ t _) = hasHoleMsg e -- need to check hasHoleMsg if deferring all type errors
isHoleExpr (Tick _ e)     = isHoleExpr e
isHoleExpr _              = False                -- and not only typed holes

isPatError :: CoreExpr -> Bool
isPatError (Case e _ t _) = case getPatErr e of
                                Just pe -> True
                                _ -> False
isPatError (Tick _ e)     = isPatError e
isPatError e              = isPatErrVar e

isPatErrVar :: CoreExpr -> Bool
isPatErrVar (Var v) = isErrVar "patError" v
isPatErrVar _       = False

isTyError :: CoreExpr -> Bool
isTyError (Case e _ t _) = case getTypErr e of
                          Just _ -> not (hasHoleMsg e) -- check if we have a type error that is not a hole
isTyError (Tick _ e)     = isTyError e
isTyError _              = False

getHoleMsg :: CoreExpr -> String
getHoleMsg e = concat [getLitString l | str@(Lit l) <- universe e, isTypedHolErrMsg l]

isHoleVar :: Var -> Bool
isHoleVar v = take 4 (getOccString v) == "hole"

isHoleVarExpr :: CoreExpr -> Bool
isHoleVarExpr (Var v)    = isHoleVar v
isHoleVarExpr (Tick _ e) = isHoleVarExpr e -- any expression might be wrapped in ticks   
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
isTyOrTyVar (Type _)   = True
isTyOrTyVar (Var v)    = isTyVar v
isTyOrTyVar (Tick _ e) = isTyOrTyVar e
isTyOrTyVar _          = False

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


isVar :: CoreExpr -> Bool
isVar (Var _)    = True
isVar (Tick _ e) = isVar e
isVar _          = False

isErrVar :: String -> Var -> Bool
isErrVar s v = getOccString v == s

isVarMod :: Var -> Bool
isVarMod v = "$trModule" == take 9 (getOccString v)

hasHoleMsg :: CoreExpr -> Bool
hasHoleMsg e = not $ null [l | Lit l <- universe e, isTypedHolErrMsg l]

isTypedHolErrMsg :: Literal -> Bool
isTypedHolErrMsg (LitString l) = f $ lines $ utf8DecodeByteString l
    where f ls | length ls > 1 = "hole:" `elem` (words (ls !! 1))
               | otherwise     = False
isTypedHolErrMsg _ = False

isAppToHole :: CoreExpr -> Bool
-- | Check if a lambda is applied to a hole
isAppToHole = \case
    (App f args) | isHoleVarExpr args -> True
    (Tick _ e) -> isAppToHole e
    (Lam v e)  -> isAppToHole e
    _          -> False
