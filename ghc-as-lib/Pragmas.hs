{-# LANGUAGE FlexibleContexts #-}

module Pragmas where 

import Text.Parsec
import Data.List 
import Debug.Trace

-- | Pragma parser
parsePragmas :: Stream s m Char => ParsecT s u m [(String, String)]
parsePragmas = many (try pPragma)  
  where
    pStartPragma = manyTill anyChar (try (string "{-#"))
    pEndPragma   = string "#-}" 
    pPragma      = (,) <$> (pStartPragma >> space >> many upper)
                       <*> (manyTill anyChar (try pEndPragma))

-- | Pragma parser
parseInProgPragmas :: Stream s m Char => ParsecT s u m [(String, String, SourcePos)]
parseInProgPragmas = many (try pPragma)
    where pStartPragma = manyTill anyChar (try (string "{-#"))
          pEndPragma   = string "#-}" 
          pPragma      = do 
              fst <- pStartPragma >> space >> many upper
              snd <- manyTill anyChar (try pEndPragma)
              pos <- getPosition
              pure (fst,snd,pos)
  
decSourcePos :: SourcePos -> Int -> SourcePos 
decSourcePos pos n = setSourceColumn pos col 
  where col = sourceColumn pos - n  

-- | Read the description pragma e.g., {-# DESC Use the higher-order function ... #-}
-- Multiple descriptions are concatenated (separated by newlines)
readDescription :: FilePath -> IO String
readDescription file = readFile file >>= return . parseDescription file 

parseDescription :: FilePath -> String -> String
parseDescription file txt =
    either (const "No description") f res
  where
    res  = runParser parsePragmas () file txt
    f    = intercalate "\n" . \ps -> [s | ("DESC", s) <- ps]

readFeedback :: FilePath -> IO [(String,SourcePos)]
readFeedback file = readFile file >>= return . parseFeedback file 


parseFeedback :: FilePath -> String -> [(String,SourcePos)]
parseFeedback file txt =
    either (const []) f res
  where
    res  = runParser parseInProgPragmas () file txt
    f ps = [(s,pos) | ("F", s, pos) <- ps]

readFeedbackFromFit :: FilePath -> String -> IO String 
readFeedbackFromFit file s = readFile file >>= return . parseFeedbackFromFit file s  


parseFeedbackFromFit :: FilePath -> String -> String -> String 
parseFeedbackFromFit file fit txt = either (const "No") id res
  where
    feedback = parseFeedback file fit 
    res  = runParser parseFit (fit,feedback) file txt 

parseFit :: Stream s m Char => ParsecT s (String,[(String,SourcePos)]) m String 
parseFit = do 
    (s,fb) <- getState
    (manyTill anyChar (try (string s))) -- need to find the correct location, this approach would fail if a function is apllued manu times
    endpos <- getPosition
    let startPos = trace ("pos" ++ show endpos) decSourcePos endpos (length s)
        fit = filter ((startPos ==) . snd) fb 
    pure (show endpos) --(fst (head fit))

{- -- | Pragma parser
parsePragmas :: Stream s m Char => ParsecT s u m [(String, String, SourcePos)]
parsePragmas = many (try pPragma)  
  where
    pStartPragma = spaces >> string "{-#"
    pEndPragma   = string "#-}" 
    pPragma      = do 
        fst <- (pStartPragma >> space >> many upper)
        snd <- (manyTill anyChar (try pEndPragma))
        thd <- getPosition 
        pure (fst,snd,thd)

-- | Read the description pragma e.g., {-# DESC Use the higher-order function ... #-}
-- Multiple descriptions are concatenated (separated by newlines)
readDescription :: FilePath -> IO [(String,SourcePos)]
readDescription file = readFile file >>= return . parseDescription file 

parseDescription :: FilePath -> String -> [(String, SourcePos)]
parseDescription file txt = either (const [("No desc", undefined)]) f res
  where
    res  = runParser parsePragmas () file txt
    f :: [(String,String,SourcePos)] -> [(String,SourcePos)]
    f ps = [(s,p) | ("DESC", s, p) <- ps] -}