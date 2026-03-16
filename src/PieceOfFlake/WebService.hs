module PieceOfFlake.WebService where

import Data.Aeson ( FromJSON(parseJSON), ToJSON(toJSON), encode )
import PieceOfFlake.Flake ( FlakeUrl, FetcherId )
import PieceOfFlake.Prelude
import Text.Show ( Show(show) )
import Yesod.Core
    ( ToContent(..),
      ToTypedContent(..),
      typeJson,
      TypedContent(TypedContent) )


newtype FetcherSecret = FetcherSecret Text deriving (Eq, Generic, FromJSON, ToJSON)
instance Show FetcherSecret where
  show _ = "****"

data FetcherHeartbeat
  = FetcherHeartbeat
  { fetcherSecret :: FetcherSecret
  , fetcherId :: FetcherId
  , workingOn :: FlakeUrl
  } deriving (Show, Eq, Generic)

instance FromJSON FetcherHeartbeat
instance ToJSON FetcherHeartbeat

newtype Period = Period { unPeriod :: Second } deriving newtype (Show, Eq, Ord, Num)

instance FromJSON Period where
  parseJSON x = Period . fromMicroseconds <$> parseJSON x
instance ToJSON Period where
  toJSON = toJSON . toMicroseconds . unPeriod

data FetcherHeartbeatPeriod
data NoSubmitionHeartbeatSec
data FetcherAutoConfig
  = FetcherAutoConfig
    { heartbeatPeriod :: Tagged FetcherHeartbeatPeriod Period
    , httpMinTimeout :: Tagged NoSubmitionHeartbeatSec Period
    }
    deriving (Show, Eq, Generic)

instance FromJSON FetcherAutoConfig
instance ToJSON FetcherAutoConfig
instance ToContent FetcherAutoConfig where
  toContent = toContent . encode
instance ToTypedContent FetcherAutoConfig where
  toTypedContent = TypedContent typeJson . toContent
