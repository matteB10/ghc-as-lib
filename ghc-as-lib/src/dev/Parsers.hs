{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- This module rexposes wrapped parsers from the GHC API. Along with
-- returning the parse result, the corresponding annotations are also
-- returned such that it is then easy to modify the annotations and print
-- the result.
--
----------------------------------------------------------------------------
module Parsers (
        -- * Utility
          Parser
        , ParseResult
        -- * Basic Parsers
        , parseDecl
        , parseWith
        ) where

import Data.Functor (void)

import qualified GHC hiding (parseModule)
import GHC.Paths (libdir)
import qualified Control.Monad.IO.Class as GHC
import qualified GHC.Data.FastString    as GHC
import qualified GHC.Data.StringBuffer  as GHC
import qualified GHC.Parser.Errors      as GHC
import qualified GHC.Driver.Session     as GHC
import qualified GHC.Parser             as GHC
import qualified GHC.Parser.Header      as GHC
import qualified GHC.Parser.Lexer       as GHC
import qualified GHC.Parser.PostProcess as GHC
import qualified GHC.Types.SrcLoc       as GHC
import qualified GHC.Driver.Env        as GHC
import qualified GHC.Driver.Phases     as GHC
import qualified GHC.Driver.Pipeline   as GHC hiding (unP)
import qualified GHC.Fingerprint.Type  as GHC
import qualified GHC.Settings          as GHC
import qualified GHC.Types.SourceError as GHC
import qualified GHC.Types.SourceFile  as GHC
import qualified GHC.Utils.Error       as GHC
import qualified GHC.Utils.Fingerprint as GHC
import qualified GHC.Utils.Outputable  as GHC
import qualified GHC.Types.Error as GHCT



import GHC.Types.SrcLoc (mkSrcSpan, mkSrcLoc)
import GHC.Data.FastString (mkFastString)



import qualified GHC.LanguageExtensions as LangExt
import qualified GHC.Types.Error as GHC hiding (getErrorMessages)
import qualified GHC.Driver.Config as GHC
import GHC.Parser.Lexer (PState, getErrorMessages, ParseResult (..), P (unP), initParserState)
import qualified GHC.Data.Bag as GHC
import Data.Maybe (catMaybes)
import GHC.Plugins ( mkSrcLoc, mkSrcSpan, mkFastString)
import GHC.Plugins hiding ((<>), initDynFlags)
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import GHC.Parser.Errors (PsError (PsError))
import GHC.Data.Bag (Bag)
import GHC (LHsDecl, GhcPs)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config (initParserOpts)
import GHC.Parser (parseDeclaration)

--let declsss = mapM (parseDecl dflags' fp . T.unpack) (T.splitOn (T.pack ";") (T.pack "f :: Int \nf = 1+1"))
--liftIO $ print declsss

type Parser a = DynFlags -> FilePath -> String -> Either (Bag PsError) a

instance Show PsError where
  show (PsError d sp _) = "Parse Error"


runParser :: P a -> DynFlags -> FilePath -> String -> ParseResult a
runParser parser flags filename str = unP parser parseState
    where
      location   = mkRealSrcLoc (mkFastString filename) 1 1
      buffer     = stringToStringBuffer str
      parseState = initParserState (initParserOpts flags) buffer location

parseDecl :: Parser (LHsDecl GhcPs)
parseDecl df fp = parseWith df fp parseDeclaration



parseWith :: DynFlags
          -> FilePath
          -> P a
          -> String
          -> Either (Bag PsError) a
parseWith dflags fileName parser s =
  case runParser parser dflags fileName s of
    PFailed pst -> Left (getErrorMessages pst)
    POk _ pmod  -> Right pmod


-- ---------------------------------------------------------------------
{-p :: PState
p = undefined 
-- | Wrapper function which returns Annotations along with the parsed
-- element.
parseWith :: GHC.DynFlags
          -> FilePath
          -> GHC.P w
          -> String
          -> ParseResult w
parseWith dflags fileName parser s =
  case runParser parser dflags fileName s of
    GHC.PFailed pst 
      -> Left (GHC.getErrorMessages pst)
    GHC.POk _ pmod
      -> Right pmod

type PSErrMsg = GHC.Bag GHC.PsError


parseWithECP :: (GHC.DisambECP w)
          => GHC.DynFlags
          -> FilePath
          -> GHC.P GHC.ECP
          -> String
          -> ParseResult (GHC.LocatedA w)
parseWithECP dflags fileName parser s =
    case runParser (parser >>= \p -> GHC.runPV $ GHC.unECP p) dflags fileName s of
      GHC.PFailed pst
        -> Left (GHC.getErrorMessages pst)
      GHC.POk _ pmod
        -> Right pmod

-- ---------------------------------------------------------------------

runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
    where
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      parseState = GHC.initParserState (GHC.initParserOpts flags) buffer location

-- ---------------------------------------------------------------------

-- | Provides a safe way to consume a properly initialised set of
-- 'DynFlags'.
--
-- @
-- myParser fname expr = withDynFlags (\\d -> parseExpr d fname expr)
-- @
withDynFlags :: LibDir -> (GHC.DynFlags -> a) -> IO a
withDynFlags libdir action = ghcWrapper libdir $ do
  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags dflags
  return (action dflags)

-- ---------------------------------------------------------------------

parseFile :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult (GHC.Located GHC.HsModule)
parseFile = runParser GHC.parseModule

-- ---------------------------------------------------------------------

type LibDir = FilePath

type ParseResult a = Either PSErrMsg a

type Parser a = GHC.DynFlags -> FilePath -> String
                -> ParseResult a

parseExpr :: Parser (GHC.LHsExpr GHC.GhcPs)
parseExpr df fp = parseWithECP df fp GHC.parseExpression

parseImport :: Parser (GHC.LImportDecl GHC.GhcPs)
parseImport df fp = parseWith df fp GHC.parseImport

parseType :: Parser (GHC.LHsType GHC.GhcPs)
parseType df fp = parseWith df fp GHC.parseType

-- safe, see D1007
parseDecl :: Parser (GHC.LHsDecl GHC.GhcPs)
parseDecl df fp = parseWith df fp GHC.parseDeclaration

parseStmt :: Parser (GHC.ExprLStmt GHC.GhcPs)
parseStmt df fp = parseWith df fp GHC.parseStatement

parsePattern :: Parser (GHC.LPat GHC.GhcPs)
parsePattern df fp = parseWith df fp GHC.parsePattern
-- ---------------------------------------------------------------------
--
-- | This entry point will also work out which language extensions are
-- required and perform CPP processing if necessary.
--
-- @
-- parseModule = parseModuleWithCpp defaultCppOptions
-- @
--
-- Note: 'GHC.ParsedSource' is a synonym for 'GHC.Located' ('GHC.HsModule' 'GhcPs')
parseModule :: LibDir -> FilePath -> IO (ParseResult GHC.ParsedSource)
parseModule libdir file = parseModuleWithCpp libdir defaultCppOptions file


-- | This entry point will work out which language extensions are
-- required but will _not_ perform CPP processing.
-- In contrast to `parseModoule` the input source is read from the provided
-- string; the `FilePath` parameter solely exists to provide a name
-- in source location annotations.
parseModuleFromString
  :: LibDir -- GHC libdir
  -> FilePath
  -> String
  -> IO (ParseResult GHC.ParsedSource)
parseModuleFromString libdir fp s = ghcWrapper libdir $ do
  dflags <- initDynFlagsPure fp s
  return $ parseModuleFromStringInternal dflags fp s

-- | Internal part of 'parseModuleFromString'.
parseModuleFromStringInternal :: Parser GHC.ParsedSource
parseModuleFromStringInternal dflags fileName str =
  let (str1, lp) = stripLinePragmas str
      res        = case runParser GHC.parseModule dflags fileName str1 of
        GHC.PFailed pst
          -> Left (GHC.getErrorMessages pst)
        GHC.POk     _  pmod
          -> Right (lp, dflags, pmod)
  in  postParseTransform res

----------- PREPROCESS

stripLinePragmas :: String -> (String, [GHC.LEpaComment])
stripLinePragmas = unlines' . unzip . findLines . lines
  where
    unlines' (a, b) = (unlines a, catMaybes b)

findLines :: [String] -> [(String, Maybe GHC.LEpaComment)]
findLines = zipWith checkLine [1..]

checkLine :: Int -> String -> (String, Maybe GHC.LEpaComment)
checkLine line s
  |  "{-# LINE" `isPrefixOf` s =
       let (pragma, res) = getPragma s
           size   = length pragma
           mSrcLoc = mkSrcLoc (mkFastString "LINE")
           ss     = mkSrcSpan (mSrcLoc line 1) (mSrcLoc line (size+1))
       in (res, Just $ mkLEpaComment pragma (GHC.spanAsAnchor ss) (GHC.realSrcSpan ss))
  -- Deal with shebang/cpp directives too
  -- x |  "#" `isPrefixOf` s = ("",Just $ Comment ((line, 1), (line, length s)) s)
  |  "#!" `isPrefixOf` s =
    let mSrcLoc = mkSrcLoc (mkFastString "SHEBANG")
        ss = mkSrcSpan (mSrcLoc line 1) (mSrcLoc line (length s))
    in
    ("",Just $ mkLEpaComment s (GHC.spanAsAnchor ss) (GHC.realSrcSpan ss))
  | otherwise = (s, Nothing)

getPragma :: String -> (String, String)
getPragma [] = error "Input must not be empty"
getPragma s@(x:xs)
  | "#" `isPrefixOf` s = ("#-}", "   " ++ drop 3 s)
  | otherwise =
      let (prag, remline) = getPragma xs
      in (x:prag, ' ':remline)



parseModuleWithOptions :: LibDir -- ^ GHC libdir
                       -> FilePath
                       -> IO (ParseResult GHC.ParsedSource)
parseModuleWithOptions libdir fp =
  parseModuleWithCpp libdir defaultCppOptions fp -}
{-

-- | Parse a module with specific instructions for the C pre-processor.
parseModuleWithCpp
  :: LibDir -- ^ GHC libdir
  -> CppOptions
  -> FilePath -- ^ File to be parsed
  -> IO (ParseResult GHC.ParsedSource)
parseModuleWithCpp libdir cpp fp = do
  res <- parseModuleEpAnnsWithCpp libdir cpp fp
  return $ postParseTransform res

-- ---------------------------------------------------------------------

-- | Low level function which is used in the internal tests.
-- It is advised to use 'parseModule' or 'parseModuleWithCpp' instead of
-- this function.
{- parseModuleEpAnnsWithCpp
  :: LibDir -- ^ GHC libdir
  -> CppOptions
  -> FilePath -- ^ File to be parsed
  -> IO
       ( Either
           GHC.ErrorMessages
           ([GHC.LEpaComment], GHC.DynFlags, GHC.ParsedSource)
       )
parseModuleEpAnnsWithCpp libdir cppOptions file = ghcWrapper libdir $ do
  dflags <- initDynFlags file
  parseModuleEpAnnsWithCppInternal cppOptions dflags file -}



-- | Internal function. Exposed if you want to muck with DynFlags
-- before parsing.
parseModuleEpAnnsWithCppInternal
  :: GHC.GhcMonad m
  => CppOptions
  -> GHC.DynFlags
  -> FilePath
  -> m
       ( Either
           GHC.ErrorMessages
           ([GHC.LEpaComment], GHC.DynFlags, GHC.ParsedSource)
       )
parseModuleEpAnnsWithCppInternal cppOptions dflags file = do
  let useCpp = GHC.xopt LangExt.Cpp dflags
  (fileContents, injectedComments, dflags') <-
    if useCpp
      then do
        (contents,dflags1) <- getPreprocessedSrcDirect cppOptions file
        cppComments <- getCppTokensAsComments cppOptions file
        return (contents,cppComments,dflags1)
      else do
        txt <- GHC.liftIO $ readFileGhc file
        let (contents1,lp) = stripLinePragmas txt
        return (contents1,lp,dflags)
  return $
    case parseFile dflags' file fileContents of
      GHC.PFailed pst
        -> Left (GHC.getErrorMessages pst)
      GHC.POk _ pmod
        -> Right $ (injectedComments, dflags', fixModuleTrailingComments pmod)

-- | Internal function. Exposed if you want to muck with DynFlags
-- before parsing. Or after parsing.
postParseTransform
  :: Either a ([GHC.LEpaComment], GHC.DynFlags, GHC.ParsedSource)
  -> Either a (GHC.ParsedSource)
postParseTransform parseRes = fmap mkAnns parseRes
  where
    -- TODO:AZ perhaps inject the comments into the parsedsource here already
    mkAnns (_cs, _, m) = fixModuleTrailingComments m

fixModuleTrailingComments :: GHC.ParsedSource -> GHC.ParsedSource
fixModuleTrailingComments (GHC.L l p) = GHC.L l p'
  where
    an' = case GHC.hsmodAnn p of
      (GHC.EpAnn a an ocs) -> GHC.EpAnn a an (rebalance (GHC.am_decls an) ocs)
      unused -> unused
    p' = p { GHC.hsmodAnn = an' } 
    -- p'  = error $ "fixModuleTrailingComments: an'=" ++ showAst an'

    rebalance :: GHC.AnnList -> GHC.EpAnnComments -> GHC.EpAnnComments
    rebalance al cs = cs'
      where
        cs' = case GHC.al_close al of
          Just (GHC.AddEpAnn _ (GHC.EpaSpan ss)) ->
            let
              pc = GHC.priorComments cs
              fc = GHC.getFollowingComments cs
              bf (GHC.L anc _) = GHC.anchor anc > ss
              (prior,f) = break bf fc
              cs'' = GHC.EpaCommentsBalanced (pc <> prior) f
            in cs''
          _ -> cs

-- | Internal function. Initializes DynFlags value for parsing.
--
-- Passes "-hide-all-packages" to the GHC API to prevent parsing of
-- package environment files. However this only works if there is no
-- invocation of `setSessionDynFlags` before calling `initDynFlags`.
-- See ghc tickets #15513, #15541.

-- | Internal function. Default runner of GHC.Ghc action in IO.
ghcWrapper :: LibDir -> GHC.Ghc a -> IO a
ghcWrapper libdir a =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut
    $ GHC.runGhc (Just libdir) a

initDynFlags :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlags file = do
  -- Based on GHC backpack driver doBackPack
  dflags0         <- GHC.getSessionDynFlags
  let parser_opts0 = GHC.initParserOpts dflags0
  src_opts        <- GHC.liftIO $ GHC.getOptionsFromFile dflags0 file
  (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 src_opts
  -- Turn this on last to avoid T10942
  let dflags2 = dflags1 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  -- Prevent parsing of .ghc.environment.* "package environment files"
  (dflags3, _, _) <- GHC.parseDynamicFlagsCmdLine
    dflags2
    [GHC.noLoc "-hide-all-packages"]
  _ <- GHC.setSessionDynFlags dflags3
  return dflags3

-- | Requires GhcMonad constraint because there is
-- no pure variant of `parseDynamicFilePragma`. Yet, in constrast to
-- `initDynFlags`, it does not (try to) read the file at filepath, but
-- solely depends on the module source in the input string.
--
-- Passes "-hide-all-packages" to the GHC API to prevent parsing of
-- package environment files. However this only works if there is no
-- invocation of `setSessionDynFlags` before calling `initDynFlagsPure`.
-- See ghc tickets #15513, #15541.
initDynFlagsPure :: GHC.GhcMonad m => FilePath -> String -> m GHC.DynFlags
initDynFlagsPure fp s = do
  -- AZ Note: "I" below appears to be Lennart Spitzner
  -- I was told we could get away with using the unsafeGlobalDynFlags.
  -- as long as `parseDynamicFilePragma` is impure there seems to be
  -- no reason to use it.
  dflags0 <- GHC.getSessionDynFlags
  let parser_opts0 = GHC.initParserOpts dflags0
  let pragmaInfo = GHC.getOptions dflags0 (GHC.stringToStringBuffer $ s) fp
  (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 pragmaInfo
  -- Turn this on last to avoid T10942
  let dflags2 = dflags1 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  -- Prevent parsing of .ghc.environment.* "package environment files"
  (dflags3, _, _) <- GHC.parseDynamicFlagsCmdLine
    dflags2
    [GHC.noLoc "-hide-all-packages"]
  _ <- GHC.setSessionDynFlags dflags3
  return dflags3

-- ---------------------------------------------------------------------

data CppOptions = CppOptions
                { cppDefine :: [String]    -- ^ CPP #define macros
                , cppInclude :: [FilePath] -- ^ CPP Includes directory
                , cppFile :: [FilePath]    -- ^ CPP pre-include file
                }

defaultCppOptions :: CppOptions
defaultCppOptions = CppOptions [] [] []



getCppTokens ::
     [(GHC.Located GHC.Token, String)]
  -> [(GHC.Located GHC.Token, String)]
  -> [(GHC.Located GHC.Token, String)]
  -> [(GHC.Located GHC.Token, String)]
getCppTokens directiveToks origSrcToks postCppToks = toks
  where
    locFn (GHC.L l1 _,_) (GHC.L l2 _,_) = compare (rs l1) (rs l2)
    m1Toks = mergeBy locFn postCppToks directiveToks

    -- We must now find the set of tokens that are in origSrcToks, but
    -- not in m1Toks

    -- GHC.Token does not have Ord, can't use a set directly
    origSpans = map (\(GHC.L l _,_) -> rs l) origSrcToks
    m1Spans = map (\(GHC.L l _,_) -> rs l) m1Toks
    missingSpans = Set.fromList origSpans Set.\\ Set.fromList m1Spans

    missingToks = filter (\(GHC.L l _,_) -> Set.member (rs l) missingSpans) origSrcToks

    missingAsComments = map mkCommentTok missingToks
      where
        mkCommentTok :: (GHC.Located GHC.Token,String) -> (GHC.Located GHC.Token,String)
        mkCommentTok (GHC.L l _,s) = (GHC.L l (GHC.ITlineComment s (makeBufSpan l)),s)

    toks = mergeBy locFn directiveToks missingAsComments

-- ---------------------------------------------------------------------

tokeniseOriginalSrc ::
  GHC.GhcMonad m
  => GHC.RealSrcLoc -> GHC.ParserOpts -> GHC.StringBuffer
  -> m [(GHC.Located GHC.Token, String)]
tokeniseOriginalSrc startLoc flags buf = do
  let src = stripPreprocessorDirectives buf
  case GHC.lexTokenStream flags src startLoc of
    GHC.POk _ ts -> return $ GHC.addSourceToTokens startLoc src ts
    GHC.PFailed pst -> parseError pst

-- ---------------------------------------------------------------------

-- | Strip out the CPP directives so that the balance of the source
-- can tokenised.
stripPreprocessorDirectives :: GHC.StringBuffer -> GHC.StringBuffer
stripPreprocessorDirectives buf = buf'
  where
    srcByLine = lines $ sbufToString buf
    noDirectivesLines = map (\line -> case line of '#' : _ -> ""; _ -> line) srcByLine
    buf' = GHC.stringToStringBuffer $ unlines noDirectivesLines

-- ---------------------------------------------------------------------

sbufToString :: GHC.StringBuffer -> String
sbufToString sb@(GHC.StringBuffer _buf len _cur) = GHC.lexemeToString sb len

-- ---------------------------------------------------------------------
getPreprocessedSrcDirect :: (GHC.GhcMonad m)
                         => CppOptions
                         -> FilePath
                         -> m (String, GHC.DynFlags)
getPreprocessedSrcDirect cppOptions src =
    (\(s,_,d) -> (s,d)) <$> getPreprocessedSrcDirectPrim cppOptions src

getPreprocessedSrcDirectPrim :: (GHC.GhcMonad m)
                              => CppOptions
                              -> FilePath
                              -> m (String, GHC.StringBuffer, GHC.DynFlags)
getPreprocessedSrcDirectPrim cppOptions src_fn = do
  hsc_env <- GHC.getSession
  let dfs = GHC.hsc_dflags hsc_env
      new_env = hsc_env { GHC.hsc_dflags = injectCppOptions cppOptions dfs }
  r <- GHC.liftIO $ GHC.preprocess new_env src_fn Nothing (Just (GHC.Cpp GHC.HsSrcFile))
  case r of
    Left err -> error $ showErrorMessages err
    Right (dflags', hspp_fn) -> do
      buf <- GHC.liftIO $ GHC.hGetStringBuffer hspp_fn
      txt <- GHC.liftIO $ readFileGhc hspp_fn
      return (txt, buf, dflags')

--showErrorMessages :: GHC.Messages a -> String
showErrorMessages :: GHC.Messages GHC.DecoratedSDoc -> String
showErrorMessages msgs =
  GHC.renderWithContext GHC.defaultSDocContext
    $ GHC.vcat
    $ GHC.pprMsgEnvelopeBagWithLoc
    $ GHCT.getErrorMessages
    $ msgs

injectCppOptions :: CppOptions -> GHC.DynFlags -> GHC.DynFlags
injectCppOptions CppOptions{..} dflags =
  foldr addOptP dflags (map mkDefine cppDefine ++ map mkIncludeDir cppInclude ++ map mkInclude cppFile)
  where
    mkDefine = ("-D" ++)
    mkIncludeDir = ("-I" ++)
    mkInclude = ("-include" ++)


addOptP :: String -> GHC.DynFlags -> GHC.DynFlags
addOptP   f = alterToolSettings $ \s -> s
          { GHC.toolSettings_opt_P   = f : GHC.toolSettings_opt_P s
          , GHC.toolSettings_opt_P_fingerprint = fingerprintStrings (f : GHC.toolSettings_opt_P s)
          }
alterToolSettings :: (GHC.ToolSettings -> GHC.ToolSettings) -> GHC.DynFlags -> GHC.DynFlags
alterToolSettings f dynFlags = dynFlags { GHC.toolSettings = f (GHC.toolSettings dynFlags) }

fingerprintStrings :: [String] -> GHC.Fingerprint
fingerprintStrings ss = GHC.fingerprintFingerprints $ map GHC.fingerprintString ss

-- ---------------------------------------------------------------------

-- | Get the preprocessor directives as comment tokens from the
-- source.
getPreprocessorAsComments :: FilePath -> IO [(GHC.Located GHC.Token, String)]
getPreprocessorAsComments srcFile = do
  fcontents <- readFileGhc srcFile
  let directives = filter (\(_lineNum,line) -> case line of '#' : _ -> True; _ -> False)
                    $ zip [1..] (lines fcontents)

  let mkTok (lineNum,line) = (GHC.L l (GHC.ITlineComment line (makeBufSpan l)),line)
       where
         start = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum 1
         end   = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum (length line)
         l = GHC.mkSrcSpan start end

  let toks = map mkTok directives
  return toks

makeBufSpan :: GHC.SrcSpan -> GHC.PsSpan
makeBufSpan ss = pspan
  where
    bl = GHC.BufPos 0
    pspan = GHC.PsSpan (GHC.realSrcSpan ss) (GHC.BufSpan bl bl)

-- ---------------------------------------------------------------------

parseError :: (GHC.MonadIO m) => GHC.PState -> m b
parseError pst = do
     let
       -- (warns,errs) = GHC.getMessages pst dflags
     -- throw $ GHC.mkSrcErr (GHC.unitBag $ GHC.mkPlainErrMsg dflags sspan err)
     -- GHC.throwErrors (fmap GHC.mkParserErr (GHC.getErrorMessages pst))
     GHC.throwErrors (fmap _ (GHC.srcErrorMessages pst))

-- ---------------------------------------------------------------------

readFileGhc :: FilePath -> IO String
readFileGhc file = do
  buf@(GHC.StringBuffer _ len _) <- GHC.hGetStringBuffer file
  return (GHC.lexemeToString buf len)
-}
-- ---------------------------------------------------------------------

-- Copied over from MissingH, the dependency cause travis to fail

{- | Merge two sorted lists using into a single, sorted whole,
allowing the programmer to specify the comparison function.

QuickCheck test property:

prop_mergeBy xs ys =
    mergeBy cmp (sortBy cmp xs) (sortBy cmp ys) == sortBy cmp (xs ++ ys)
          where types = xs :: [ (Int, Int) ]
                cmp (x1,_) (x2,_) = compare x1 x2

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _cmp [] ys = ys
mergeBy _cmp xs [] = xs
mergeBy cmp (allx@(x:xs)) (ally@(y:ys))
        -- Ordering derives Eq, Ord, so the comparison below is valid.
        -- Explanation left as an exercise for the reader.
        -- Someone please put this code out of its misery.
    | (x `cmp` y) <= EQ = x : mergeBy cmp xs ally
    | otherwise = y : mergeBy cmp allx ys

-}