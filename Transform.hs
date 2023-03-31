
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use lambda-case" #-}

module Transform where

import GHC.Core ( Expr(..), CoreExpr, CoreBndr, Bind(..), CoreProgram, Alt (..), valArgCount, CoreBind, AltCon (..))
import GHC.Types.Name ( mkOccName, getOccString, mkInternalName, isDataConName, isSystemName)
import qualified GHC.Types.Name.Occurrence as Occ
--import qualified GHC.Types.Var as Var
import GHC.Types.Var
    ( Var(..),
      isTyVar,
      tyVarKind,
      isTcTyVar,
      isId,
      isTyCoVar,
      mkLocalVar,
      idInfo,
      idDetails,
      Id(..),
      isGlobalId,
      mustHaveLocalBinding,
      setVarType,
      mkGlobalVar,
      isLocalVar,
      setVarType, isTyVarBinder, setIdNotExported )
import GHC.Core.TyCo.Rep (Type(..), Kind, CoercionR, TyLit (StrTyLit), AnonArgFlag (VisArg))
import GHC.Core.Type (eqType)
import GHC.Data.FastString (fsLit, mkFastString)
import GHC.Types.Unique
import GHC.Core.TyCon (TyCon)

import GHC.Types.SrcLoc ( mkGeneralSrcSpan, srcLocSpan, GenLocated )
import GHC.Types.Id.Info (IdDetails(VanillaId), vanillaIdInfo, pprIdDetails)
import GHC.Plugins (IdEnv, showSDocUnsafe, Literal (LitString), mkDefaultCase, needsCaseBinding, mkLocalId)
import GHC.Base ((<|>))
import GHC.Data.Maybe (fromJust, liftMaybeT)
import GHC.Utils.Outputable (Outputable(ppr))
import GHC.Iface.Ext.Types (pprIdentifier)
import GHC.Tc.Utils.TcType (isTyConableTyVar)
import GHC.Core.Utils (exprType)

import qualified Data.Text as T
import Control.Monad.Trans.State
import Data.Data (Data)
import Control.Monad (when, (>=>))
import Data.Maybe (isNothing, isJust)
import Debug.Trace ( trace )
import qualified Data.Map as Map
import Data.Map (Map(..), insert, lookup)
import Data.List (intersect, delete, (\\))
import Control.Comonad.Identity (Identity(runIdentity), (<<=))
import Control.Lens (universeOf, universeOn)

import Data.Generics.Uniplate.Data
    ( rewriteBi, transformBi, transformBiM, universeBi, rewriteBiM, Biplate )
import GHC.Types.Literal (Literal)
import GHC.Utils.Encoding (utf8DecodeByteString)
import GHC.Data.Bag (Bag)
import GHC.Parser.Annotation (SrcSpanAnnA)
import GHC.Hs (GhcTc, XUnboundVar)
import GHC.Hs.Binds (HsBindLR)
import GHC (HsExpr (..), Name)
import GHC.Tc.Types.Evidence (HoleExprRef(..))


import Utils ( isHole', isHoleExpr, isVarMod, varNameUnique )
import Similar ( Similar((~==)) )
import Data.Type.Equality (apply)
import Instance ( BiplateFor ) 
import qualified Data.Map as M
import Data.Generics.Uniplate (transform)



normalise :: String -> CoreProgram -> CoreProgram
-- | Combine normalising transformations
normalise funname = alpha funname . rewriteRec funname . etaReduce . replaceHoles . removeModInfo

normalise' :: String -> CoreProgram -> CoreProgram
normalise' funname = rewriteRec funname . etaReduce . removeModInfo

type Uq = (Char , Int)


data St = St {
         env  :: Map.Map Var Var
        ,freshUq   :: (Char,Int)
        ,freshNum  :: Int
        ,freshHNum :: Int
        ,exerName  :: String
        }


type Ctx a = State St a


freshVar :: Type -> Ctx Id 
freshVar t = do 
    (c,i) <- gets freshUq
    j <- gets freshNum
    let name = "c_" ++ show j 
    let id = mkVanillaVar name (c,i) t  
    modify $ \s -> s {env = Map.insert id id (env s), freshNum = j+1, freshUq=(c,i+1)} -- update map 
    return id

mkVanillaVar :: String -> (Char,Int) -> Type -> Var 
mkVanillaVar n uq t = mkLocalVar id_det name t t id_inf
        where id_det = VanillaId 
              uq'    = uncurry mkUnique uq 
              name   = mkInternalName uq' (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              id_inf = vanillaIdInfo 

incUq :: (Char,Int) -> (Char,Int)
incUq (c,i) = (c, i+1)



newVar :: Id -> Ctx Id
newVar id = do
    let uq@(c,i) = unpkUnique $ getUnique id
    j <- gets freshNum
    let name = "n_"++show j
    let id' = makeVar id name
    modify $ \s -> s {env = Map.insert id id' (env s), freshNum = j+1} -- update map 
    return id'

newHoleVar :: Id -> Type -> Ctx Id
newHoleVar id t = do
    let uq@(c,i) = unpkUnique $ getUnique id
    j <- gets freshHNum
    let name = "hole_"++ show j
    let id' = setVarType (makeVar id name) t
    modify $ \s -> s {env = Map.insert id' id' (env s), freshHNum = j+1} -- update map 
    return id'


makeGlobVar :: Id -> String -> Id
makeGlobVar id n = mkGlobalVar id_det name typ id_inf
        where uq     = getUnique id
              id_det = idDetails id
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              typ    = varType id
              id_inf = idInfo id



makeVar :: Id -> String -> Id
makeVar id n = mkLocalVar id_det name mult typ id_inf
        where uq     = getUnique id
              id_det = idDetails id
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              mult   = varType id
              typ    = varType id
              id_inf = idInfo id 


replaceHoles :: CoreProgram -> CoreProgram
-- | Replace expressions representing holes with hole variables of the same type 
replaceHoles cs = evalState (tr cs) (St {env = Map.empty, freshNum = 0, freshHNum = 0, exerName = ""})
    where tr :: CoreProgram -> Ctx CoreProgram
          tr = transformBiM $ \case 
                    c@(Case e v t a) | isHoleExpr c -> let id = fromJust (getTypErr e)
                                                       in newHoleVar id t >>= \i -> return $ Var i
                                     | otherwise -> return c
                    e -> return e

alphaWCtxt :: String -> CoreProgram -> (CoreProgram, Map.Map Var Var)
-- | Do renaming and return map of renamed variables        
alphaWCtxt fname cs = (evalState st initialState, map)
    where st@(StateT f) = mapM alphaR cs
          map = env $ execState st initialState
          initialState = (St {env = Map.empty, freshNum = 0, freshHNum = 0, exerName = fname})

alpha :: String -> CoreProgram -> CoreProgram
-- | Rename all local non-type variables, replace holes with variable s
alpha fname cs = evalState cs' (St {env = Map.empty, freshNum = 0, freshHNum = 0, exerName = fname})
    where cs' = mapM alphaR cs


alphaR :: Bind Var -> Ctx (Bind Var)
alphaR (NonRec v e) = do
        v' <- renameVar v
        NonRec v' <$> aRename e
alphaR (Rec es) = do
        vars <- mapM (renameVar . fst) es
        exps <- mapM (aRename . snd) es
        return $ Rec (zip vars exps)

renameVar :: Id -> Ctx Id
renameVar v = --trace ("var:" ++ show v ++ " is tyvar: " ++ show (isTyVar v) ++ " musthavebind: " ++ show (mustHaveLocalBinding v) ++ " islocal: " ++ show (isLocalVar v) ++ " isSysName "++show (isSystemName (varName v))) 
            do
            env <- gets env
            name <- gets exerName
            case Map.lookup v env of
                Just n  -> return n
                Nothing -> checkNew v name
    where checkNew v n | varNameUnique v == n = return v
                       | isGlobalId v = return v
                       | isTyVar v    = return v
                       | isTyCoVar v  = return v   ------ this is a problem in the simplifier pass, since local helper gets global ids
                                                   ------ but we dont want to rename global id's like (:)
                       | otherwise    = newVar v


aRename :: Expr Var -> Ctx (Expr Var)
aRename v@(Var id)     | isHole' v = return v
                       | otherwise  = Var <$> renameVar id
aRename t@(Type _)     = return t
aRename l@(Lit _)      = return l
aRename (App e arg)    = do
        e' <- aRename e
        arg' <- aRename arg
        return $ App e' arg'
aRename l@(Lam b e)      = do --  trace ("Lambda var:" ++ show b ++ " isTyVar: " ++ show (isTyVar b)) $
        b' <- renameVar b
        e' <- aRename e
        return $ Lam b' e'
aRename c@(Case e v t a) = do
                            e' <- aRename e
                            v' <- renameVar v
                            a' <- renameAlt a
                            return $ Case e' v' t a'
aRename (Cast e co)    = trace ("DO we ever find coercion " ++ show co) $
                         do
        e' <- aRename e
        return $ Cast e' co
aRename (Let b e)      = do
    b' <- alphaR b
    e' <- aRename e
    return $ Let b' e'
aRename (Tick ct e)    = aRename e >>= \e' -> return (Tick ct e')
aRename x              = return x

renameAlt :: [Alt Var] -> Ctx [Alt Var]
renameAlt = mapM renameAlt'
    where renameAlt' :: Alt Var -> Ctx (Alt Var)
          renameAlt' (Alt ac vs e) = do
            vs' <- mapM renameVar vs
            e' <- aRename e
            return $ Alt ac vs' e'


getTypErr :: Expr Var -> Maybe Id
getTypErr (Var id)       | getOccString id == "typeError" = Just id
getTypErr (App e arg)    = getTypErr e <|> getTypErr arg
getTypErr (Lam b e)      = getTypErr e
getTypErr (Case e _ _ _) = getTypErr e
getTypErr (Let b e)      = getTypErrB b <|> getTypErr e
getTypErr x              = Nothing

getTypErrB :: CoreBind -> Maybe Id
getTypErrB (NonRec _ e) = getTypErr e
getTypErrB (Rec es) = case filter isNothing k of
                    [] -> Nothing
                    l  -> Just (fromJust $ head l)
        where k = map (getTypErr . snd) es



removeModInfo :: CoreProgram -> CoreProgram
removeModInfo = concatMap removeModInf
    where removeModInf :: Bind Var -> [Bind Var]
          removeModInf (NonRec v e) | isVarMod v = []
          removeModInf b                         = [b]


etaReduce :: BiplateFor CoreProgram => CoreProgram -> CoreProgram
etaReduce = rewriteBi etaRed


etaRed :: Expr Var ->  Maybe (Expr Var)
-- | eta reduction, e.g., \x -> f x => f
etaRed (Lam v (App f args)) = 
   case args of
      Var v' | v == v' -> return f 
      _                -> Nothing
etaRed _ = Nothing


rewriteRec :: String -> CoreProgram -> CoreProgram
-- | Inline recursive binders as let-recs when appropriate
rewriteRec fn cs = evalState cs' (St {env = Map.empty, freshNum = 0, freshHNum = 0, exerName = fn})
    where cs' = rewriteRecs cs


rewriteRecs :: CoreProgram -> Ctx CoreProgram
-- | Rewrite top-level Recursive binders as Let-Recs in a NonRec binder (if not used somewhere in the program)
--   Inline Recs as Let-Recs into NonRecs 
rewriteRecs [] = return []
rewriteRecs (b:bs) = case b of
    rr@(Rec ((v,e):ls)) -> do
        if --trace ("BN" ++ show bn ++ "V" ++ show v) 
            any (insB v) bs  --  if used somewhere else in program, inline it there
                                 --  might be bad for performanc
            then 
                let (newBinds, rest) = inline rr bs 
                in rewriteRecs rest >>= \bs' -> return $ newBinds ++ bs'
            else do
                fresh <- newVar v
                rewriteRecs bs >>= \bs' -> return $ NonRec v (Let (Rec ((fresh,e):ls)) (Var fresh)):bs' -- rewrite as a nonrec

    nr@(NonRec v e) -> do
        if --trace ("BN" ++ show bn ++ "V" ++ show v) 
            any (insB v) bs  --  if used somewhere else in program, inline it there
                             --  might be bad for performance
            then
                let (newBinds, rest) = inline nr bs 
                in rewriteRecs rest >>= \bs' -> return $ newBinds ++ bs'
            else
                rewriteRecs bs >>= \bs' -> return $ nr:bs'

ins :: Data (Expr Var) => Var -> Expr Var -> Bool
-- | Find if a variable is used somewhere in an expr
ins n e = or [v==n | v <- universeBi e :: [Var]]

insB :: Data (Bind Var) => Var -> Bind Var -> Bool
-- | Find if a variable is used somewhere in a binder
insB n b = or [v==n | v <- universeBi b :: [Var]]


inline :: Bind Var -> [Bind Var] -> ([Bind Var],[Bind Var])
-- | Inline a binder and return remaining binders 
inline b bs = let ls  = getBind bs (getBindTopVar b) 
                  b'  = setBindTopVar (changeScope (getBindTopVar b)) b -- change scope to local of binder variable if inlined 
                  bs' = insertBind b' ls 
                  in (bs', delete b (bs \\ ls))

changeScope :: Var -> Var 
changeScope v | isId v  && isGlobalId v = setIdNotExported $ mkLocalId (varName v) (varMult v) (varType v)
              | isId v                  = setIdNotExported v 
              | otherwise = v 


setBindTopVar :: Var -> Bind Var -> Bind Var 
setBindTopVar v = transformBi $ \e -> case e :: CoreExpr of 
        (Var _) -> Var v
        e       -> e 

getBindTopVar :: Bind Var -> Var 
-- | Get variable of a binder 
getBindTopVar (NonRec v _) = v 
getBindTopVar (Rec ((v,e):es)) = v 


getBind :: [CoreBind] -> Var -> [Bind Var]
-- | Get all binders containing a certain variable
getBind binds v = [r | r <- binds, v `insB` r]


bindNames :: Bind Var -> [Name]
-- | Return all names used in a binder 
bindNames = \case
    Rec ((v,e):es) -> varName v:bindNamesE e
    NonRec v e     -> varName v:bindNamesE e
    where bindNamesE :: Expr Var -> [Name]
          bindNamesE e = concat [bindNames n | Let n e' <- universeBi e]


insertBind :: Bind Var -> [Bind Var] -> [Bind Var]
insertBind r@(Rec es) bs = map insertB bs  -- dont handle when es not empty
    where insertB (NonRec b (Lam x e)) = NonRec b (Lam x (Let r e))
          insertB (NonRec b e) = NonRec b (Let r e)
          insertB (Rec _)      = error "not sure about inlining recs into recs"
insertBind n@(NonRec v e) bs = map insertB bs -- inline another nonrec 
    where insertB = transformBi $ \case
            (Var v') | v == v' -> e
            e -> e

--- EXPERIMENTAL STUFF BELOW
----------------------------------------------------

addDefaultCase :: CoreProgram -> CoreProgram 
addDefaultCase p = evalState (pm p) initSt 
    where pm :: CoreProgram -> Ctx CoreProgram 
          pm = transformBiM addDefCase 
          
initSt :: St 
initSt = St {env = Map.empty, freshNum = 0, freshHNum = 0, exerName = " ", freshUq = ('x',1)}

addDefCase :: Expr Var -> Ctx (Expr Var)
addDefCase ex@(Lam v e) | isTyVar v                     = Lam v <$> addDefCase e
                            | needsCaseBinding (varType v) e = addCase e v -- rather tests whether
                                                                           -- needs to use a case rather than let bind
addDefCase e = return e 


addCase :: Expr Var -> Var -> Ctx (Expr Var)
addCase e v = do
    let t = varType v  
    fresh <- freshVar t
    wild <- freshVar t  -- should have same type as the case 
    e' <- subst fresh v e 
    return $ Lam fresh $ mkDefaultCase (Var fresh) wild e' 

subst :: Var -> Var -> CoreExpr -> Ctx CoreExpr
subst v v' = transformBiM (sub v v')  

sub :: Var -> Var -> CoreExpr -> Ctx CoreExpr
sub v v' = \case 
    (Var id) | id == v' -> do 
        modify $ \s -> s {env = Map.insert v v' (env s)}
        return (Var v)
    e -> return e 

-- Core Utils 
-- mkSingleAltCase
-- needsCaseBInding
-- bindNonRec
-- mkAltExpr -- make case alternatives 
-- | Extract the default case alternative
-- findDefault :: [Alt b] -> ([Alt b], Maybe (Expr b))
-- -- | Find the case alternative corresponding to a particular
-- constructor: panics if no such constructor exists
-- findAlt :: AltCon -> [Alt b] -> Maybe (Alt b)

-- check if we can use diffBinds from Core.Utils to find small diffs 
-- diffExpr instead of my similarity relation? need an RnEnv2

-- mkLamTypes :: [Var] -> Type -> Type
-- can this be used for beta-expansioN???????

-- applyTypeToArgs :: HasDebugCallStack => SDoc -> Type -> [CoreExpr] -> Type
-- ^ Determines the type resulting from applying an expression with given type
--- to given argument expressions.
-- Do I need to do this backwards when eta-reducing?