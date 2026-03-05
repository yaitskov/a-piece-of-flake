{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.Flake.Repo where

import Data.Acid ( AcidState )
import Data.Aeson ( FromJSON, ToJSON )
import Control.Concurrent (threadDelay)
import Control.Monad.Logger ( logError, logInfo, WriterLoggingT )
import PieceOfFlake.Acid ( AcidFlakes )
import PieceOfFlake.Flake
    ( Flake(FlakeFetched, SubmittedFlake, flakeUrl,
            FlakeIsBeingFetched, BadFlake),
      FlakeUrl,
      IpAdr,
      MetaFlake,
      FetcherId(..) )
import PieceOfFlake.CmdArgs
import PieceOfFlake.Index ( FlakeIndex )
import PieceOfFlake.Prelude hiding (Map, show)
import PieceOfFlake.Prelude qualified as P
import PieceOfFlake.Stm ( newTQueueIO, readTQueue, writeTQueue, TQueue, atomicalog )
import StmContainers.Map ( insert, lookup, newIO, Map )

data FlakeRepo
  = FlakeRepo
  { flakes :: Map FlakeUrl Flake
  , fetcherSecret :: FetcherSecret
  , acidFlakes :: AcidState AcidFlakes
  , fetcherIps :: Map FetcherId IpAdr
  , fetcherQueue :: TQueue (Maybe FlakeUrl)
  , indexerQueue :: TQueue FlakeUrl
  , fetcherQueueLen :: TVar Int
  , indexerQueueLen :: TVar Int
  , flakeIndex :: TVar FlakeIndex
  , acidQueue :: TQueue (FlakeUrl, Flake)
  }

mkFlakeRepo :: MonadIO m => FetcherSecret -> TVar FlakeIndex -> Map FlakeUrl Flake -> AcidState AcidFlakes -> m FlakeRepo
mkFlakeRepo fetSec fi flakesMap acidFlakeStorage = do
  liftIO $
    FlakeRepo flakesMap fetSec acidFlakeStorage <$>
      newIO <*>
      newTQueueIO <*>
      newTQueueIO <*>
      newTVarIO 0 <*>
      newTVarIO 0 <*>
      pure fi <*>
      newTQueueIO

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
            $(logInfo) $ "Fetcher queue increased to " <> P.show fql
            let f = SubmittedFlake fu now ip in do
              lift $ insert f fu fr.flakes
              pure $ Right f
      Just f ->
        pure $ Right f

popFlakeSubmition :: MonadIO m => FlakeRepo -> FetcherId -> m (Maybe FlakeUrl)
popFlakeSubmition fr ftid = do
  now <- liftIO getCurrentTime
  atomicalog (popFlakeSubmitionStm ftid fr now)

popFlakeSubmitionStm  ::
  FetcherId ->
  FlakeRepo ->
  UTCTime ->
  WriterLoggingT STM (Maybe FlakeUrl)
popFlakeSubmitionStm ftid fr now = do
  fSub <- lift $ readTQueue fr.fetcherQueue
  lift $ modifyTVar' fr.fetcherQueueLen (\x -> x - 1)
  fql <- lift $ readTVar fr.fetcherQueueLen
  $(logInfo) $ "Fetcher queue decreased to " <> P.show fql
  case fSub of
    Nothing -> pure fSub
    Just fu ->
      lift (lookup fu fr.flakes) >>= \case
        Just (SubmittedFlake { flakeUrl })
          | flakeUrl == fu -> do
            lift $ insert (FlakeIsBeingFetched flakeUrl now ftid) fu fr.flakes
            pure $ Just flakeUrl
          | otherwise -> do
            $(logError) $ "Error flake url mismatch " <> P.show fu <> " <> " <> P.show flakeUrl
            popFlakeSubmitionStm ftid fr now
        Just ufs -> do
          $(logError) $ "Expected SumbittedFlake state but:" <> P.show ufs
          popFlakeSubmitionStm ftid fr now
        Nothing -> do
          $(logError) $ "Error flake " <> P.show fu <> " is missing in map"
          popFlakeSubmitionStm ftid fr now

data FetcherReq
  = FetcherReq
  { fetcherId :: FetcherId
  , fetcherResponse :: Maybe (FlakeUrl, Either Text MetaFlake)
  , fetcherSecret :: FetcherSecret
  } deriving (Show, Eq, Generic)

instance FromJSON FetcherReq
instance ToJSON FetcherReq

-- | Store meta data for flake and ask for next flake submition
addFetchedFlake :: MonadIO m =>
  FlakeRepo ->
  FetcherId ->
  (FlakeUrl, Either Text MetaFlake) ->
  m (Maybe FlakeUrl)
addFetchedFlake fr ftid (fu, fetchedFlake) = do
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
                $(logInfo) $ "Fetching flake " <> P.show fu <> " failed in " <> P.show (duration now past)
                popFlakeSubmitionStm ftid fr now
              Right meta -> do
                let f = FlakeFetched fu now meta
                lift $ insert f fu fr.flakes
                $(logInfo) $ "Flake " <> P.show fu <> " is fetched in " <> P.show (duration now past)
                lift $ writeTQueue fr.indexerQueue fu
                lift $ writeTQueue fr.acidQueue (fu, f)
                lift $ modifyTVar' fr.indexerQueueLen (1 +)
                iql <- lift $ readTVar fr.indexerQueueLen
                $(logInfo) $ "Indexer queue increased to " <> P.show iql
                popFlakeSubmitionStm ftid fr now
          | otherwise -> do
              $(logError) $ "Error flake url mismatch " <> P.show fu <> " <> " <> P.show exFu
              pure Nothing
        Just ufs -> do
          $(logError) $ "Expected FlakeIsBeingFetched state but: " <> P.show ufs
          pure Nothing
        Nothing -> do
          $(logError) $ "Error flake " <> P.show fu <> " is missing in map"
          pure Nothing

sendEmtpyFlakeSubmition :: MonadIO m => FlakeRepo -> Int -> m ()
sendEmtpyFlakeSubmition fr d = do
  liftIO $ threadDelay d
  atomicalog $ do
    fql <- lift $ readTVar fr.fetcherQueueLen
    when (fql == 0) $ do
      $(logInfo) "Send empty Flake Submition"
      lift $ do
        writeTQueue fr.fetcherQueue Nothing
        modifyTVar' fr.fetcherQueueLen (1 +)
  sendEmtpyFlakeSubmition fr d
