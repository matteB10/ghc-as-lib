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