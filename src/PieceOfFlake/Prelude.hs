module PieceOfFlake.Prelude
  ( module X
  , duration
  , readFileTxt
  , toMs
  , PoF
  , getCurrentTime
  , alt
  ) where

import Control.Exception as X (IOException)
import Control.Lens as X ((^.), _2, _1)
import Control.Monad.Catch as X (Handler (..))
import Control.Monad.Logger as X

import Data.Aeson as X ( FromJSON, ToJSON )
import Data.Tagged as X
import Data.Time.Clock as X hiding (getCurrentTime)
import Data.Time.Clock qualified as C
import Data.Time.Units as X
import Debug.TraceEmbrace as X hiding (PackageName, Error, a)
import GHC.TypeLits as X (symbolVal)
import Refined as X (unrefine, refine, refineTH, Refined, FromTo)
import Relude as X hiding (Handle, intercalate)
import System.IO.Unsafe as X (unsafePerformIO)
import UnliftIO as X (MonadUnliftIO, finally, catchAny, catch, stringException, throwIO)

duration :: UTCTime -> UTCTime -> Double
duration a b = realToFrac $ diffUTCTime a b

readFileTxt :: MonadIO m => FilePath -> m Text
readFileTxt fp = decodeUtf8 <$>  readFileBS fp

toMs :: TimeUnit a => a -> Int
toMs = fromIntegral . toMicroseconds

type PoF m = (MonadLogger m, MonadIO m, MonadUnliftIO m)

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO C.getCurrentTime

alt :: [a] -> [a] -> [a]
alt a b = case a of [] -> b ; o -> o
