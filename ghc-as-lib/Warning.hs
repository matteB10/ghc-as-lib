module Warning where 

import GHC ( Severity (..), SrcSpan ) 
import GHC.Driver.Session (WarnReason(..))
import GHC.Types.Error (SDoc)
import GHC.Driver.Ppr (showSDoc)
import GHC.Utils.Logger (LogAction, defaultLogAction)
import Data.IORef (IORef, modifyIORef)
import Prelude hiding (span)
import Control.Monad (when)


data Warning = GhcWarn {reason :: WarnReason,
                        sev :: Severity,
                        span :: SrcSpan,
                        doc :: SDoc}
    deriving (Show) 

instance Eq WarnReason where
  (Reason flag) == (Reason flag') = flag == flag'
  (ErrReason f) == (ErrReason f') = f == f'
  NoReason == NoReason = True
  _ == _               = False

instance Eq Warning where 
  -- | Equality of warnings based on warning reason
  w == w' = reason w == reason w' 

instance Ord Warning where 
  w > w' = sev w > sev w' 
  w <= w' = sev w <= sev w' 

instance Ord Severity where 
  SevFatal > s = True 
  SevError > s = case s of 
        SevFatal -> False 
        _        -> True 
  SevWarning > s = case s of 
      SevFatal -> False 
      SevError -> False 
      _        -> True 
  _ <= SevFatal = True 
  SevWarning <= SevError = True 
      
uniqWarns :: Warning -> Warning -> Bool 
-- | compare warnings based on reason and source location
uniqWarns w w' = reason w == reason w' && span w == span w' 

writeWarnings :: IORef [Warning] -> LogAction -> LogAction
-- | write warnings to IORef
writeWarnings ref action dflags reason sev span doc = do
  modifyIORef ref (GhcWarn reason sev span doc:)
  noAction dflags reason sev span doc 
  -- replace noAction with defaultLogAction to output errors and warnings to stdout/stderr

getWarnLoc :: [Warning] -> [SrcSpan]
-- | Get location from warning
getWarnLoc = map Warning.span 

noAction :: LogAction
noAction _ _ _ _ _ = return () 