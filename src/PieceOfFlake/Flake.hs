module PieceOfFlake.Flake where

import StmContainers.Map
import Data.Time.Clock
import PieceOfFlake.Prelude hiding (Map)
import Yesod.Core.Json

newtype FlakeUrl = FlakeUrl Text deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, Hashable)

newtype FlakeRepo
  = FlakeRepo
  { flakes :: Map FlakeUrl Flake
  }

data Flake
  = SubmittedFlake { flakeUrl :: FlakeUrl
                   , submittedAt :: UTCTime
                   }
  deriving (Show, Eq, Generic)


mkFlakeRepo :: MonadIO m => m FlakeRepo
mkFlakeRepo = FlakeRepo <$> liftIO newIO

trySubmitFlakeToRepo :: MonadIO m => FlakeRepo -> FlakeUrl -> m ()
trySubmitFlakeToRepo (FlakeRepo m) fu = do
  now <- liftIO getCurrentTime
  atomically $ do
    lookup fu m >>= \case
      Nothing -> insert (SubmittedFlake fu now) fu m
      Just _ -> pure ()
