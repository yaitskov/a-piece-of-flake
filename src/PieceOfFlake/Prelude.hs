module PieceOfFlake.Prelude
  ( module X
  , duration
  , readFileTxt
  ) where

import Data.Time.Clock as X
import Data.Tagged as X
import Relude as X hiding (Handle, intercalate)
import Debug.TraceEmbrace as X hiding (PackageName, Error, a)
import System.IO.Unsafe as X (unsafePerformIO)
import UnliftIO as X (MonadUnliftIO, finally, catchAny, stringException, throwIO)

duration :: UTCTime -> UTCTime -> Double
duration a b = realToFrac $ diffUTCTime a b

readFileTxt :: MonadIO m => FilePath -> m Text
readFileTxt fp = decodeUtf8 <$>  readFileBS fp
