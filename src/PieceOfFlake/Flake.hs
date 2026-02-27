{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.Flake where

import Control.Concurrent.STM.TQueue
import Data.Aeson
import Data.Map.Strict qualified as M
import Data.Time.Clock
import PieceOfFlake.Index
import PieceOfFlake.Prelude hiding (Map)
import StmContainers.Map
import Yesod.Core


newtype FlakeUrl = FlakeUrl Text deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, Hashable)

newtype Architecture = Architecture Text
  deriving newtype
  ( Show, Eq, Ord, Hashable
  , ToJSON, FromJSON, ToJSONKey, FromJSONKey
  )
newtype PackageName = PackageName Text
  deriving newtype
  ( Show, Eq, Ord, Hashable
  , ToJSON, FromJSON, ToJSONKey, FromJSONKey
  )

data PackageInfo
  = PackageInfo
  { description :: Text
  , license :: Text
  , name :: PackageName
  , unfree :: Bool
  , platforms :: [ Text ]
  , broken :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON PackageInfo
instance FromJSON PackageInfo

data MetaFlake
  = MetaFlake
  { description :: Maybe Text
  , packages :: M.Map Architecture (M.Map PackageName PackageInfo)
  , rev :: Text
  , flakeDeps :: [ FlakeUrl ]
  } deriving (Show, Eq, Generic)

instance ToJSON MetaFlake
instance FromJSON MetaFlake

newtype IpAdr = IpAdr Text  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)
newtype FetcherId = FetcherId Text deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

data Flake
  = SubmittedFlake
  { flakeUrl :: FlakeUrl
  , submittedAt :: UTCTime
  , submittedFrom :: IpAdr
  }
  | FlakeIsBeingFetched
  { flakeUrl :: FlakeUrl
  , submitionFetchedAt :: UTCTime
  , fetcherId :: FetcherId
  }
  | BadFlake
  { flakeUrl :: FlakeUrl
  , fetcherRespondedAt :: UTCTime
  , error :: Text
  }
  | FlakeFetched
  { flakeUrl :: FlakeUrl
  , uploadedAt :: UTCTime
  , meta :: MetaFlake
  }
  | FlakeIndexed
  { flakeUrl :: FlakeUrl
  , indexedAt :: UTCTime
  , meta :: MetaFlake
  }
  deriving (Show, Eq, Generic)

instance ToJSON Flake
instance FromJSON Flake

instance ToContent Flake where
  toContent = toContent . encode
instance ToTypedContent Flake where
  toTypedContent = TypedContent typeJson . toContent

data FlakeRepo
  = FlakeRepo
  { flakes :: Map FlakeUrl Flake
  , fetcherIps :: Map FetcherId IpAdr
  , fetcherQueue :: TQueue FlakeUrl
  , fetcherQueueLen :: TVar Int
  , flakeIndex :: TVar FlakeIndex
  }


mkFlakeRepo :: MonadIO m => m FlakeRepo
mkFlakeRepo = liftIO
  (FlakeRepo <$> newIO <*> newIO <*> newTQueueIO <*> newTVarIO 0 <*> newTVarIO emptyFlakeIndex)

trySubmitFlakeToRepo :: MonadIO m => IpAdr -> FlakeRepo -> FlakeUrl -> m (Either Text Flake)
trySubmitFlakeToRepo ip fr fu = do
  now <- liftIO getCurrentTime
  atomically $ do
    lookup fu fr.flakes >>= \case
      Nothing -> do
        ql <- readTVar fr.fetcherQueueLen
        if ql > 1000
          then pure $ Left "Submition Queue is full"
          else do
            modifyTVar' fr.fetcherQueueLen (1 +)
            writeTQueue fr.fetcherQueue fu
            let f = SubmittedFlake fu now ip in do
              insert f fu fr.flakes
              pure $ Right f
      Just f ->
        pure $ Right f
