module PieceOfFlake.Flake where

import Data.Time.Clock
import PieceOfFlake.Prelude
import Yesod.Core.Json

data Flake
  = SubmittedFlake { flakeUrl :: Text
                   , submittedAt :: UTCTime
                   }
  deriving (Show, Eq, Generic)

newtype FlakeUrl = FlakeUrl Text deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)
