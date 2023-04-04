{-# LANGUAGE LambdaCase #-}
module MyPlugin (plugin, install) where
import GHC.Plugins
import GHC (getSessionDynFlags, runGhc)
import GHC.Core.Opt.Monad (getRuleBase)
import qualified Data.Map as Map
import Control.Monad.Trans.State
import GHC.Types.Unique (unpkUnique)
import qualified GHC.Types.Name.Occurrence as Occ
import Data.Generics.Uniplate.Data
import Data.Maybe (fromJust, isNothing)
import GHC.Core.Lint (interactiveInScope)
import GHC.Runtime.Context (InteractiveContext(InteractiveContext), extendInteractiveContextWithIds)
import GHC.Core.Opt.Arity (etaExpand, exprEtaExpandArity, etaExpandAT)
import GHC.Tc.Validity (arityErr)
import Control.Monad.Trans.Class (MonadTrans(lift))
import GHC.Driver.Monad (modifySessionM, modifySession, GhcMonad (..))
import GHC.Paths (libdir)

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install,
  pluginRecompile = impurePlugin
}


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  --reinitializeGlobals
  return (CoreDoPluginPass "Replace Holes" repHoles : 
          --CoreDoPluginPass "Eta expand"    etaExpPass :
          todo)



etaExpPass :: ModGuts -> CoreM ModGuts
-- | Eta expand ~experimental
etaExpPass mg@ModGuts {mg_binds = prog} = do
        prog' <- etaExp prog 
        return $ mg {mg_binds = prog'}
      where etaExp :: CoreProgram -> CoreM CoreProgram
            etaExp = transformBiM $ \case
              e@(Var id) -> return e 
              e@(App e' arg) -> eta e 
              e -> return e 
              -- e@(App f arg) -> undefined  
            eta e = do 
              dflags <- getDynFlags 
              let at = exprEtaExpandArity dflags e 
              return $ etaExpandAT at e
            --eta id = etaExpand (arityInfo (idInfo id))

modifyEnv :: HscEnv -> CoreM ()  
-- this does not work , since we create a separate session 
modifyEnv env = liftIO $ runGhc (Just libdir) $ do modifySession (const env)

repHoles :: ModGuts -> CoreM ModGuts
repHoles mg@ModGuts {mg_binds = prog} = do
  env <- getHscEnv
  let inscopeVars = interactiveInScope $ hsc_IC env
  (prog',inscopeVars') <- replaceHoles inscopeVars prog
  let ic = extendInteractiveContextWithIds ic inscopeVars'
  let env' = env {hsc_IC = ic}
  modifyEnv env'
  --liftIO $ putStr $ showSDocUnsafe $ ppr $ interactiveInScope ic 
  return $ mg {mg_binds = prog'}
        where replaceHoles :: [Var] -> CoreProgram -> CoreM (CoreProgram,[Var])
              -- | Replace expressions representing holes with hole variables of the same type 
              replaceHoles is cs = runStateT (tr cs) is
                where tr :: CoreProgram -> StateT [Var] CoreM CoreProgram
                      tr = transformBiM $ \case
                        c@(Case e v t a) | isHoleExpr c -> let id = fromJust (getTypErr e)
                                                           in newHoleVar t >>= \i -> return $ Var i
                                         | otherwise -> return c
                        e -> return e

newHoleVar :: Type -> StateT [Var] CoreM Id
newHoleVar t = do
    vars <- get
    let is = mkInScopeSet $ mkUniqSet vars
    uq <- lift getUniqueM
    --let uq = unsafeGetFreshLocalUnique is
    let name = "hole"
        id  = setIdExported $ setVarType (makeVar uq t name) t
        is' = extendInScopeSet is id
    put (eltsUFM $ getUniqSet  $ getInScopeVars is')
    return id


-- check function newLocal :: FastString -> ScaledType -> UniqSM Id from GHC.Types.Id.Make 

{- newHoleVar :: Id -> Type -> Ctx Id
newHoleVar id t = do
    let uq@(c,i) = unpkUnique $ getUnique id
    j <- freshHNum
    let name = "hole_"++ show j
    let id' = setVarType (makeVar id name) t
    modify $ \s -> s {env = Map.insert id' id' (env s), freshHNum = j+1} -- update map 
    return id' -}

getTypErr :: Expr Var -> Maybe Var
getTypErr e = head [Just v | (Var v) <- universe e, getOccString v == "typeError"]

makeVar :: Unique -> Type -> String -> Id
makeVar uq t n = mkLocalVar id_det name typ mult id_inf
        where id_det = VanillaId
              name   = mkInternalName uq (mkOccName Occ.varName n) (mkGeneralSrcSpan (mkFastString ("Loc " ++ n)))
              mult   = t
              typ    = t
              id_inf = vanillaIdInfo

type Ctx a = State St a

data St = St {vars :: Map.Map Id Id}

isHoleExpr :: Expr Var -> Bool
isHoleExpr (Case e _ t _) = not (all isNothing (containsTErr e))
isHoleExpr _              = False



containsTErr :: Expr Var -> [Maybe (Expr Var)]
containsTErr (Var id)       | isVarTErr id = [Just $ Var id]
                            | otherwise = [Nothing]
containsTErr (Lit l)        = [Nothing]
containsTErr (App e arg)    = containsTErr e ++ containsTErr arg
containsTErr (Lam b e)      = containsTErr e
containsTErr (Case e _ _ _) = containsTErr e
containsTErr (Cast e co)    = containsTErr e
containsTErr _              = [Nothing]


isVarTErr :: Var -> Bool
isVarTErr v = getOccString v == "typeError"



--instance Show Var where 
--  show = getOccString

{- {-# LANGUAGE LambdaCase #-}
{-# language DeriveDataTypeable #-}
module MyPlugin where 

import GHC.Plugins (Plugin, defaultPlugin, CorePlugin, installCoreToDos, pluginRecompile, CommandLineOption, fromSerialized, deserializeWithData, ModGuts(..), Name, CoreExpr, Var, flattenBinds, getName, thNameToGhcName, PluginRecompile(..), showSDoc)
import GHC.Core.Opt.Monad (CoreToDo(..), CorePluginPass, bindsOnlyPass, CoreM, putMsgS, getDynFlags, errorMsg)
import GHC.Core (CoreProgram, CoreBind, Bind(..), Expr(..))
import GHC.Types.Annotations (Annotation(..), AnnTarget(..))
import GHC.Utils.Outputable (Outputable(..), SDoc, showSDocUnsafe, (<+>), ppr, text)
-- template-haskell
import qualified Language.Haskell.TH.Syntax as TH (Name)
import Data.Data (Data)
import Language.Haskell.TH (Loc, AnnTarget(..), Pragma(..), Dec(..), location, Q, Exp(..))
import Language.Haskell.TH.Syntax (liftData)
import Data.Maybe (listToMaybe)
import Data.List.HT (partitionMaybe)
import Data.Foldable (traverse_)

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install -- will be defined below
  , pluginRecompile = \_ -> pure NoForceRecompile
                       }

-- append a 'CoreDoPluginPass' at the _end_ of the 'CoreToDo' list
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = pure (todos ++ [pass])
  where
    pass = CoreDoPluginPass pname $ \ guts -> do
      let (guts', targets) = extractTargets guts
      traverse_ (\ t -> printCore guts' (tgName t)) targets
      pure guts'
    pname = "MyPlugin"


-- Print the Core representation of the expression that has the given Name
printCore :: ModGuts -> TH.Name -> CoreM ()
printCore guts thn = do
  mn <- lookupTHName guts thn
  case mn of
    Just (_, coreexpr) -> do
      dflags <- getDynFlags
      putMsgS $ showSDoc dflags (ppr coreexpr) -- GHC pretty printer
    Nothing -> do
      errorMsg $ text "Cannot find name" <+> text (show thn)

-- our annotation payload type
data Target = MkTarget { tgName :: TH.Name } deriving (Data)


inspect :: TH.Name -> Q [Dec]
inspect n = do
  annExpr <- liftData (MkTarget n)
  pure [PragmaD (AnnP ModuleAnnotation annExpr)]


extractTargets :: ModGuts -> (ModGuts, [Target])
extractTargets guts = (guts', xs)
  where
    (anns_clean, xs) = partitionMaybe findTargetAnn (mg_anns guts)
    guts' = guts { mg_anns = anns_clean }
    findTargetAnn = \case 
      (Annotation t payload) -> fromSerialized deserializeWithData payload
      _ -> (Nothing)

-- Resolve the TH.Name into a GHC Name and look this up within the list of binders in the module guts
lookupTHName :: ModGuts -> TH.Name -> CoreM (Maybe (Var, CoreExpr))
lookupTHName guts thn = thNameToGhcName thn >>= \case
  Nothing -> do
    errorMsg $ text "Could not resolve TH name" <+> text (show thn)
    pure Nothing
  Just n -> pure $ lookupNameInGuts guts n
  where
    lookupNameInGuts :: ModGuts -> Name -> Maybe (Var, CoreExpr)
    lookupNameInGuts guts n = listToMaybe
                              [ (v,e)
                              | (v,e) <- flattenBinds (mg_binds guts)
                              , getName v == n
                              ]
 -}
