module PieceOfFlake.Prelude
  ( module X
  , readFileTxt
  , toMs
  , PoF
  , alt
  , threadDelay
  , toNominal
  ) where

import Control.Concurrent qualified as CC
import Control.Exception as X (IOException)
import Control.Lens as X ((^.), _2, _1)
import Control.Monad.Catch as X (Handler (..))
import Control.Monad.Logger as X

import Data.Aeson as X ( FromJSON, ToJSON )
import Data.Tagged as X
import Data.Time.Clock.NonNegativeTimeDiff as X
import Data.Time.Units as X
import Debug.TraceEmbrace as X hiding (PackageName, Error, a, u)
import GHC.TypeLits as X (symbolVal)
import Refined as X (unrefine, refine, refineTH, Refined, FromTo)
import Relude as X hiding (Handle, intercalate)
import System.IO.Unsafe as X (unsafePerformIO)
import UnliftIO as X (MonadUnliftIO, finally, catchAny, catch, stringException, throwIO)

readFileTxt :: MonadIO m => FilePath -> m Text
readFileTxt fp = decodeUtf8 <$>  readFileBS fp

toMs :: TimeUnit a => a -> Int
toMs = fromIntegral . toMicroseconds

type PoF m = (MonadLogger m, MonadIO m, MonadUnliftIO m, ClockMonad m)


alt :: [a] -> [a] -> [a]
alt a b = case a of [] -> b ; o -> o

threadDelay :: (MonadIO m, TimeUnit tu) => tu -> m ()
threadDelay d =
  liftIO $ CC.threadDelay $ fromIntegral (convertUnit d :: Microsecond)

toNominal :: TimeUnit tu => tu -> NominalDiffTime
toNominal tu =  (fromIntegral (convertUnit tu :: Microsecond) :: NominalDiffTime) / 1_000_000
