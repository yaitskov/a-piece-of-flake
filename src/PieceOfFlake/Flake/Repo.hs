{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.Flake.Repo where

import Data.Acid ( AcidState )
import Control.Concurrent (threadDelay)
import Control.Monad.Logger ( logError, logInfo, WriterLoggingT )
import PieceOfFlake.Acid
import PieceOfFlake.Flake
import PieceOfFlake.Index ( FlakeIndex, emptyFlakeIndex )
import PieceOfFlake.Prelude hiding (Map)
import PieceOfFlake.Stm ( newTQueueIO, readTQueue, writeTQueue, TQueue, atomicalog )
import StmContainers.Map ( insert, lookup, newIO, Map )

data FlakeRepo
  = FlakeRepo
  { flakes :: Map FlakeUrl Flake
  , acidFlakes :: AcidState AcidFlakes
  , fetcherIps :: Map FetcherId IpAdr
  , fetcherQueue :: TQueue (Maybe FlakeUrl)
  , indexerQueue :: TQueue FlakeUrl
  , fetcherQueueLen :: TVar Int
  , indexerQueueLen :: TVar Int
  , flakeIndex :: TVar FlakeIndex
  , acidQueue :: TQueue (FlakeUrl, Flake)
  }

mkFlakeRepo :: MonadIO m => AcidState AcidFlakes -> m FlakeRepo
mkFlakeRepo acidFlakeStorage = do
  flakesMap <- liftIO newIO
  flakeEntries <- reverse <$> loadFromDb acidFlakeStorage
  atomically $ mapM_ (\(k, v) -> insert v k flakesMap) flakeEntries
  liftIO $
    FlakeRepo flakesMap acidFlakeStorage <$>
      newIO <*>
      newTQueueIO <*>
      newTQueueIO <*>
      newTVarIO 0 <*>
      newTVarIO 0 <*>
      newTVarIO emptyFlakeIndex <*>
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
            $(logInfo) $ "Fetcher queue increased to " <> show fql
            let f = SubmittedFlake fu now ip in do
              lift $ insert f fu fr.flakes
              pure $ Right f
      Just f ->
        pure $ Right f


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
                let f = FlakeFetched fu now meta
                lift $ insert f fu fr.flakes
                $(logInfo) $ "Flake " <> show fu <> " is fetched in " <> show (duration now past)
                lift $ writeTQueue fr.indexerQueue fu
                lift $ writeTQueue fr.acidQueue (fu, f)
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
    fql <- lift $ readTVar fr.fetcherQueueLen
    when (fql == 0) $ do
      $(logInfo) "Send empty Flake Submition"
      lift $ do
        writeTQueue fr.fetcherQueue Nothing
        modifyTVar' fr.fetcherQueueLen (1 +)
  sendEmtpyFlakeSubmition fr d
