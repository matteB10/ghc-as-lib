{-# LANGUAGE LambdaCase #-}

module Transform.Rename where 

import GHC.Core.Utils (exprType)
import GHC.Core
    ( Bind(..), Expr(..), CoreProgram, CoreBind, Alt (..), CoreExpr ) 
import GHC.Types.Var
    ( Var(..),
      globaliseId,
      isTcTyVar,
      isTyCoVar,
      setVarName,
      setVarType )
import GHC.Types.Id
    ( isConLikeId, isJoinId, setIdInfo, Id, isGlobalId, isImplicitId ) 
import GHC.Types.Unique.Supply
    ( UniqSupply,
      listSplitUniqSupply,
      mkSplitUniqSupply,
      uniqFromSupply )
import GHC.Core.Type (eqType )
import GHC.Core.Predicate ( isEvVar ) 
import GHC.Types.Unique ( Uniquable(getUnique) ) 

import Utils.Utils
import Control.Monad.Trans.State 
import Data.Generics.Uniplate.Data 
import qualified Data.Map as Map 
import GHC.Types.Name (getSrcSpan)
import Data.Maybe (fromJust)
import GHC.Types.Id.Info
import GHC.Core.Opt.Arity (exprArity)

-- alpha renaming and other replacing transformations
------------------------------------------------------


replaceHoles :: CoreProgram -> IO CoreProgram
-- | Replace holes (case typerror) with variables
replaceHoles p = mkSplitUniqSupply 'H' >>= \us -> return (evalState (repHoles (listSplitUniqSupply us) p) 0)
    where repHoles :: [UniqSupply] -> CoreProgram -> State Int CoreProgram
          repHoles us = transformBiM $ \case
            c@(Case e v t [])
                | isHoleExpr c -> do
                                holecount <- get -- counter used to retrieve uniques from uniquesupply
                                modify $ \s -> s+1 -- increase counter
                                let id = fromJust (getTypErr e)
                                    ty = exprType c -- might be another type  
                                    uq = uniqFromSupply (us !! holecount)
                                    name = makeName ("hole" ++ show holecount) uq (getSrcSpan (varName v))
                                    id' = setIdInfo (setVarType (setVarName v name) ty) idinf
                                    idinf = setArityInfo vanillaIdInfo (exprArity e)
                                return $ Var (globaliseId id') -- make global Id since hole could be something from any scope
                | otherwise -> return c
            e -> return e

replacePatErrors :: CoreProgram -> IO CoreProgram
-- | Replace pattern errors (case patError) with variables
replacePatErrors = return . repPatErr
    where repPatErr :: CoreProgram -> CoreProgram
          repPatErr = transformBi $ \case
            c@(Case e v t []) -- we only replace paterrors with empty alternatives 
                | isPatError c -> let id = fromJust (getPatErr e)
                                      ty = exprType c -- check this type
                                      name = makeName "patError" (getUnique id) (getSrcSpan (varName v))
                                      id' = setIdInfo (setVarType (setVarName v name) ty) idinf
                                      idinf = setArityInfo vanillaIdInfo (exprArity e)
                                   in Var (globaliseId id') 
                | otherwise -> c
            e -> e

-- replacing case binders with scrutinee
replaceCaseBinds :: CoreProgram -> IO CoreProgram
-- | Substitute back the scrutinee for case binded name in case expressions
replaceCaseBinds = return . transformBi repBinds
    where repBinds :: CoreExpr -> CoreExpr
          -- | replace case result binder with scrutinee (if the case binder is "wild")
          --  e.g. Case xs of wild -> {_ -> f wild} =>  Case xs of wild -> {_ -> f xs}
          repBinds (Case e b t as) | isWild b = Case e b t (map (sub e b) as)
              where sub :: CoreExpr ->  Var -> Alt Var -> Alt Var
                    sub e v (Alt ac vars ex) = Alt ac vars (subE e v ex)
          repBinds e = e

-- variable renaming

data VarType = TopBind | CaseBind | GenVar | LamVar

data St = St {
         env  :: Map.Map Var Var
        ,topBindCount      :: Int -- to rename all top-level binders separately 
        ,freshCaseBindVar  :: Int
        ,freshVar          :: Int
        ,freshBindVar      :: Int
        ,freshLamVar       :: Int
        ,exerName  :: String
        }

type Ctx a = State St a


initSt :: St
initSt = St {env = Map.empty, topBindCount = 0, freshCaseBindVar = 0, freshVar = 0, freshBindVar = 0, freshLamVar = 0, exerName = ""}

alpha :: String -> CoreProgram -> IO (CoreProgram, Map.Map Var Var)
-- | Do renaming and return map of renamed variables        
alpha fname cs = return (prog, env state)
    where (prog,state) = runState st (initSt {exerName = fname})
          st = mapM alphaR cs
          alphaR cb = modify resetState >> alphaB cb

resetState :: St -> St
-- Reset name counters
resetState st = st {topBindCount = topBindCount st + 1,
                    freshCaseBindVar = 0,
                    freshVar = 0,
                    freshBindVar = 0}


alphaB :: CoreBind -> Ctx CoreBind
alphaB cb = do
    alpha cb
    where alpha = \case
            (NonRec v e) -> do
                v' <- renameVar TopBind v
                NonRec v' <$> aRename e
            (Rec es) -> do
                vars <- mapM (renameVar TopBind . fst) es
                exps <- mapM (aRename . snd) es
                return $ Rec (zip vars exps)

renameVar :: VarType -> Id -> Ctx Id
renameVar vty v | isSpecialVar = return v
                | otherwise = do
            env <- gets env
            name <- gets exerName
            if varNameUnique v == name 
                then return v 
                else case Map.lookup v env of
                    Just n  | varType n `eqType` varType v -> return n
                            | otherwise -> renameV vty v 
                    Nothing -> renameV vty v 
    where isSpecialVar = 
                 isGlobalId v 
              || isTyCoVar v 
              || isSpecVar v 
              || isImplicitId v
              || isTcTyVar v
              || isJoinId v 
              || isEvVar v 
              || isConLikeId v


renameV :: VarType -> Var -> Ctx Var
renameV vty v = do
    vname <- getVName vty
    let v' = setVarName v (makeName vname (getUnique v) (getSrcSpan (varName v)))
    modify $ \s -> s {env = Map.insert v v' (env s)} -- update map 
    return v'

getVName :: VarType -> Ctx String
getVName vty = do
    bc <- gets topBindCount
    case vty of
        CaseBind -> do
                i <- gets freshCaseBindVar
                modify $ \ s -> s {freshCaseBindVar = i+1}
                return ("cb"++show bc ++ show i)
        TopBind -> do
                i <- gets freshBindVar
                modify $ \ s -> s {freshBindVar = i+1}
                return ("b" ++ show bc ++ show i)
        GenVar -> do
                i <- gets freshVar
                modify $ \ s -> s {freshVar = i+1}
                return ("v" ++ show bc ++ show i)
        LamVar -> do
                i <- gets freshLamVar
                modify $ \ s -> s {freshLamVar = i+1}
                return ("l" ++ show bc ++ show i)

aRename :: CoreExpr -> Ctx CoreExpr
aRename v@(Var id)     = Var <$> renameVar GenVar id
aRename t@(Type _)     = return t
aRename l@(Lit _)      = return l
aRename (App e arg)    = do
        e' <- aRename e
        arg' <- aRename arg
        return $ App e' arg'
aRename l@(Lam b e)      = do --  trace ("Lambda var:" ++ show b ++ " isTyVar: " ++ show (isTyVar b)) $
        b' <- renameVar LamVar b
        e' <- aRename e
        return $ Lam b' e'
aRename c@(Case e v t a) = do
                            e' <- aRename e
                            v' <- renameVar CaseBind v
                            a' <- renameAlt a
                            return $ Case e' v' t a'
aRename (Cast e co)    =
                         do
        e' <- aRename e
        return $ Cast e' co
aRename (Let b e)      = do
    b' <- alphaB b
    e' <- aRename e
    return $ Let b' e'
aRename (Tick ct e)    = aRename e >>= \e' -> return (Tick ct e')
aRename x              = return x

renameAlt :: [Alt Var] -> Ctx [Alt Var]
renameAlt = mapM renameAlt'
    where renameAlt' :: Alt Var -> Ctx (Alt Var)
          renameAlt' (Alt ac vs e) = do
            vs' <- mapM (renameVar GenVar) vs
            e' <- aRename e
            return $ Alt ac vs' e'