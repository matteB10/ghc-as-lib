{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module TypeCheckW where 

---- GHC Code not exported in ghc 9.2.5 
import GHC 
import GHC.Types.Error (mkWarnMsg, WarningMessages, Messages, emptyMessages, getWarningMessages)
import GHC.Tc.Types (FrontendResult (FrontendTypecheck), TcGblEnv)
import GHC.Driver.Hooks (Hooks(..))
import GHC.Tc.Module (RenamedStuff, tcRnInstantiateSignature, tcRnMergeSignatures, getRenamedStuff)
import GHC.Types.SourceFile (HscSource(..))
import GHC.Utils.Logger (HasLogger(..), DumpFormat (FormatHaskell), dumpIfSet_dyn)
import GHC.Hs.Dump (showAstData, BlankSrcSpan (NoBlankSrcSpan), BlankEpAnnotations (..))
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types (HieASTs(getAsts), HieFile (hie_asts, hie_module))
import GHC.Iface.Ext.Debug (validateScopes, diffFile)
import GHC.Utils.Error (putMsg)
import GHC.Iface.Env (updNameCache)
import GHC.Iface.Ext.Ast (mkHieFile)
import GHC.Fingerprint (Fingerprint)
import GHC.Unit.Module.ModDetails (ModDetails)
import GHC.Stack (withFrozenCallStack, HasCallStack)
import GHC.Base (assert, when)
import GHC.Plugins (ModIface, HasDynFlags (..), text)
import Control.Monad.IO.Class (liftIO)
import GHC.Driver.Session
    ( HasDynFlags(getDynFlags),
      DumpFlag(Opt_D_ppr_debug, Opt_D_dump_rn_ast, Opt_D_dump_hie),
      dopt_set )
import Data.Maybe ( fromJust ) 
import GHC.Driver.Main
    ( getHscEnv, hscParse', hscSimpleIface', ioMsgMaybe, tcRnModule' )
import GHC.Driver.Env
    ( HscEnv(hsc_NC, hsc_hooks, hsc_dflags),
      hsc_home_unit,
      runHsc,
      Hsc(..) )
import GHC.Types.SrcLoc ( mkRealSrcLoc, realSrcLocSpan )
import GHC.Data.FastString ( mkFastString )
import GHC.Unit
    ( isHoleModule,
      homeModuleNameInstantiation,
      isHomeModule,
      mkHomeModule )
import GHC.Utils.Outputable (Outputable(..))


-- | Do Typechecking without throwing SourceError exception with -Werror
hscTypecheckAndGetWarnings :: HscEnv ->  ModSummary -> IO (FrontendResult, WarningMessages)
hscTypecheckAndGetWarnings hsc_env summary = runHsc' hsc_env $ do
  case hscFrontendHook (hsc_hooks hsc_env) of
    Nothing -> FrontendTypecheck . fst <$> hsc_typecheck False summary Nothing
    Just h  -> h summary

runHsc' :: HscEnv -> Hsc a -> IO (a, WarningMessages)
runHsc' hsc_env (Hsc hsc) = hsc hsc_env (getWarningMessages emptyMessages)

hsc_typecheck :: Bool -- ^ Keep renamed source?
              -> ModSummary -> Maybe HsParsedModule
              -> Hsc (TcGblEnv, RenamedStuff)
hsc_typecheck keep_rn mod_summary mb_rdr_module = do
    hsc_env <- getHscEnv
    let hsc_src = ms_hsc_src mod_summary
        dflags = hsc_dflags hsc_env
        home_unit = hsc_home_unit hsc_env
        outer_mod = ms_mod mod_summary
        mod_name = moduleName outer_mod
        outer_mod' = mkHomeModule home_unit mod_name
        inner_mod = homeModuleNameInstantiation home_unit mod_name
        src_filename  = ms_hspp_file mod_summary
        real_loc = realSrcLocSpan $ mkRealSrcLoc (mkFastString src_filename) 1 1
        keep_rn' = gopt Opt_WriteHie dflags || keep_rn
    massert ( isHomeModule home_unit outer_mod )
    tc_result <- if hsc_src == HsigFile && not (isHoleModule inner_mod)
        then ioMsgMaybe $ tcRnInstantiateSignature hsc_env outer_mod' real_loc
        else
         do hpm <- case mb_rdr_module of
                    Just hpm -> return hpm
                    Nothing -> hscParse' mod_summary
            tc_result0 <- tcRnModule' mod_summary keep_rn' hpm
            if hsc_src == HsigFile
                then do (iface, _, _) <- liftIO $ hscSimpleIface hsc_env tc_result0 Nothing
                        ioMsgMaybe $
                            tcRnMergeSignatures hsc_env hpm tc_result0 iface
                else return tc_result0
    -- TODO are we extracting anything when we merely instantiate a signature?
    -- If not, try to move this into the "else" case above.
    rn_info <- extract_renamed_stuff mod_summary tc_result
    return (tc_result, rn_info)
    
massert :: (HasCallStack, Applicative m) => Bool -> m ()
{-# INLINE massert #-}
massert cond = withFrozenCallStack (assert cond (pure ()))


hscSimpleIface :: HscEnv
               -> TcGblEnv
               -> Maybe Fingerprint
               -> IO (ModIface, Maybe Fingerprint, ModDetails)
hscSimpleIface hsc_env tc_result mb_old_iface
    = runHsc hsc_env $ hscSimpleIface' tc_result mb_old_iface

extract_renamed_stuff :: ModSummary -> TcGblEnv -> Hsc RenamedStuff
extract_renamed_stuff mod_summary tc_result = do
    let rn_info = getRenamedStuff tc_result

    dflags <- getDynFlags
    logger <- getLogger
    liftIO $ dumpIfSet_dyn logger dflags Opt_D_dump_rn_ast "Renamer"
                FormatHaskell (showAstData NoBlankSrcSpan NoBlankEpAnnotations rn_info)

    -- Create HIE files
    when (gopt Opt_WriteHie dflags) $ do
        -- I assume this fromJust is safe because `-fwrite-hie-file`
        -- enables the option which keeps the renamed source.
        hieFile <- mkHieFile mod_summary tc_result (fromJust rn_info)
        let out_file = ml_hie_file $ ms_location mod_summary
        liftIO $ writeHieFile out_file hieFile
        liftIO $ dumpIfSet_dyn logger dflags Opt_D_dump_hie "HIE AST" FormatHaskell (ppr $ hie_asts hieFile)

        -- Validate HIE files
        when (gopt Opt_ValidateHie dflags) $ do
            hs_env <- Hsc $ \e w -> return (e, w)
            liftIO $ do
              -- Validate Scopes
              case validateScopes (hie_module hieFile) $ getAsts $ hie_asts hieFile of
                  [] -> putMsg logger dflags $ text "Got valid scopes"
                  xs -> do
                    putMsg logger dflags $ text "Got invalid scopes"
                    mapM_ (putMsg logger dflags) xs
              -- Roundtrip testing
              file' <- readHieFile (NCU $ updNameCache $ hsc_NC hs_env) out_file
              case diffFile hieFile (hie_file_result file') of
                [] ->
                  putMsg logger dflags $ text "Got no roundtrip errors"
                xs -> do
                  putMsg logger dflags $ text "Got roundtrip errors"
                  mapM_ (putMsg logger (dopt_set dflags Opt_D_ppr_debug)) xs
    return rn_info

