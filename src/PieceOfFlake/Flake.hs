{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.Flake where

import Data.Curry
import Control.Concurrent.STM.TQueue
import Control.Monad.Logger
import Data.Aeson
import Data.Map.Strict qualified as M
import PieceOfFlake.Index
import PieceOfFlake.Prelude hiding (Map)
import StmContainers.Map
import Yesod.Core
import Control.Concurrent (threadDelay)



newtype FlakeUrl = FlakeUrl Text
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, Hashable, ToContent, ToTypedContent)

instance ToContent (Maybe FlakeUrl) where
  toContent = \case
    Just a -> toContent a
    Nothing -> toContent ("" :: ByteString)

instance ToTypedContent (Maybe FlakeUrl) where
  toTypedContent = TypedContent typeJson . toContent

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
  , fetcherQueue :: TQueue (Maybe FlakeUrl)
  , indexerQueue :: TQueue FlakeUrl
  , fetcherQueueLen :: TVar Int
  , indexerQueueLen :: TVar Int
  , flakeIndex :: TVar FlakeIndex
  }

mkFlakeRepo :: MonadIO m => m FlakeRepo
mkFlakeRepo = liftIO
  (FlakeRepo <$> newIO <*> newIO <*>
   newTQueueIO  <*> newTQueueIO <*>
   newTVarIO 0 <*> newTVarIO 0 <*>
   newTVarIO emptyFlakeIndex)

trySubmitFlakeToRepo :: MonadIO m => IpAdr -> FlakeRepo -> FlakeUrl -> m (Either Text Flake)
trySubmitFlakeToRepo ip fr fu = do
  now <- liftIO getCurrentTime
  atomicalog $ do
    lift (lookup fu fr.flakes) >>= \case
      Nothing -> do
        ql <- lift (readTVar fr.fetcherQueueLen)
        if ql > 1000
          then pure $ Left "Submition Queue is full"
          else do
            lift $ modifyTVar' fr.fetcherQueueLen (1 +)
            lift $ writeTQueue fr.fetcherQueue $ Just fu
            fql <- lift $ readTVar fr.fetcherQueueLen
            $(logInfo) $ "Fetcher queue increased to " <> show fql
            let f = SubmittedFlake fu now ip in do
              lift $ insert f fu fr.flakes
              pure $ Right f
      Just f ->
        pure $ Right f

atomicalog :: MonadIO m => WriterLoggingT STM a -> m a
atomicalog a = do
  (r, l) <- atomically (runWriterLoggingT a)
  runStderrLoggingT $ do
    mapM_ (unc4 monadLoggerLog) l
  pure r

popFlakeSubmition :: MonadIO m => FlakeRepo -> m (Maybe FlakeUrl)
popFlakeSubmition fr = do
  now <- liftIO getCurrentTime
  atomicalog (popFlakeSubmitionStm fr now)

popFlakeSubmitionStm  :: FlakeRepo -> UTCTime -> WriterLoggingT STM (Maybe FlakeUrl)
popFlakeSubmitionStm fr now = do
  fSub <- lift $ readTQueue fr.fetcherQueue
  lift $ modifyTVar' fr.fetcherQueueLen (\x -> x - 1)
  fql <- lift $ readTVar fr.fetcherQueueLen
  $(logInfo) $ "Fetcher queue decreased to " <> show fql
  case fSub of
    Nothing -> pure fSub
    Just fu ->
      lift (lookup fu fr.flakes) >>= \case
        Just (SubmittedFlake { flakeUrl })
          | flakeUrl == fu -> do
            lift $ insert (FlakeIsBeingFetched flakeUrl now $ FetcherId "fid") fu fr.flakes
            pure $ Just flakeUrl
          | otherwise -> do
            $(logError) $ "Error flake url mismatch " <> show fu <> " <> " <> show flakeUrl
            popFlakeSubmitionStm fr now
        Just ufs -> do
          $(logError) $ "Expected SumbittedFlake state but:" <> show ufs
          popFlakeSubmitionStm fr now
        Nothing -> do
          $(logError) $ "Error flake " <> show fu <> " is missing in map"
          popFlakeSubmitionStm fr now


-- | Store meta data for flake and return next flake submition
addFetchedFlake :: MonadIO m => FlakeRepo -> (FlakeUrl, Either Text MetaFlake) -> m (Maybe FlakeUrl)
addFetchedFlake fr (fu, fetchedFlake) = do
  now <- liftIO getCurrentTime
  atomicalog $ go now
  where
    go now = do
      lift (lookup fu fr.flakes) >>= \case
        Just (FlakeIsBeingFetched exFu past _fid)
          | fu == exFu ->
            case fetchedFlake of
              Left e -> do
                lift $ insert (BadFlake fu now e) fu fr.flakes
                $(logInfo) $ "Fetching flake " <> show fu <> " failed in " <> show (duration now past)
                popFlakeSubmitionStm fr now
              Right meta -> do
                lift $ insert (FlakeFetched fu now meta) fu fr.flakes
                $(logInfo) $ "Flake " <> show fu <> " is fetched in " <> show (duration now past)
                lift $ writeTQueue fr.indexerQueue fu
                lift $ modifyTVar' fr.indexerQueueLen (1 +)
                iql <- lift $ readTVar fr.indexerQueueLen
                $(logInfo) $ "Indexer queue increased to " <> show iql
                popFlakeSubmitionStm fr now
          | otherwise -> do
              $(logError) $ "Error flake url mismatch " <> show fu <> " <> " <> show exFu
              pure Nothing
        Just ufs -> do
          $(logError) $ "Expected FlakeIsBeingFetched state but: " <> show ufs
          pure Nothing
        Nothing -> do
          $(logError) $ "Error flake " <> show fu <> " is missing in map"
          pure Nothing

sendEmtpyFlakeSubmition :: MonadIO m => FlakeRepo -> Int -> m ()
sendEmtpyFlakeSubmition fr d = do
  liftIO $ threadDelay d
  atomicalog $ do
    $(logInfo) "Send empty Flake Submition"
    lift $ do
      writeTQueue fr.fetcherQueue Nothing
      modifyTVar' fr.fetcherQueueLen (1 +)
  sendEmtpyFlakeSubmition fr d
