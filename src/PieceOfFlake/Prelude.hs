module PieceOfFlake.Prelude
  ( module X
  , duration
  , readFileTxt
  , toMs
  , PoF
  ) where

import Control.Monad.Catch as X (Handler (..))
import Control.Monad.Logger as X
import Data.Aeson as X ( FromJSON, ToJSON )
import Data.Time.Clock as X
import Data.Tagged as X
import Relude as X hiding (Handle, intercalate)
import Debug.TraceEmbrace as X hiding (PackageName, Error, a)
import System.IO.Unsafe as X (unsafePerformIO)
import Data.Time.Units as X
import UnliftIO as X (MonadUnliftIO, finally, catchAny, stringException, throwIO)

duration :: UTCTime -> UTCTime -> Double
duration a b = realToFrac $ diffUTCTime a b

readFileTxt :: MonadIO m => FilePath -> m Text
readFileTxt fp = decodeUtf8 <$>  readFileBS fp

toMs :: TimeUnit a => a -> Int
toMs = fromIntegral . toMicroseconds

type PoF m = (MonadLogger m, MonadIO m, MonadUnliftIO m)
