{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.Flake.Repo where

import Control.Concurrent (threadDelay)
import Data.Acid ( AcidState )
import PieceOfFlake.Acid ( AcidFlakes )
import PieceOfFlake.Flake
    ( FetcherId,
      Flake(fetcherRespondedAt, SubmittedFlake, flakeUrl,
            FlakeIsBeingFetched, BadFlake, FlakeFetched),
      FlakeUrl(..),
      IpAdr,
      MetaFlake,
      RawFlakeUrl(..) )
import PieceOfFlake.CmdArgs
import PieceOfFlake.Index
import PieceOfFlake.Prelude hiding (Map, show)
import PieceOfFlake.Prelude qualified as P
import PieceOfFlake.Stats
import PieceOfFlake.Stm ( newTQueueIO, readTQueue, writeTQueue, TQueue, atomicalog )
import StmContainers.Map ( insert, lookup, newIO, Map )
import Text.Regex.TDFA ( (=~) )

data FlakeRepo
  = FlakeRepo
  { flakes :: Map FlakeUrl Flake
  , fetcherSecret :: FetcherSecret
  , flakeIndex :: FlakeIndex
  , wsArgs :: WsCmdArgs
  , acidFlakes :: AcidState AcidFlakes
  , repoStats :: RepoStatsF TVar
  , fetcherIps :: Map FetcherId IpAdr
  , fetcherQueue :: TQueue (Maybe FlakeUrl)
  , fetcherQueueLen :: TVar Int
  , acidQueue :: TQueue (FlakeUrl, Flake)
  }

mkFlakeRepo :: MonadIO m =>
  FetcherSecret ->
  WsCmdArgs ->
  FlakeIndex ->
  Map FlakeUrl Flake ->
  AcidState AcidFlakes ->
  RepoStatsF TVar ->
  m FlakeRepo
mkFlakeRepo fetSec cmdA fi flakesMap acidFlakeStorage rs = do
  liftIO $
    FlakeRepo flakesMap fetSec fi cmdA acidFlakeStorage rs <$>
      newIO <*>
      newTQueueIO <*>
      newTVarIO 0 <*>
      newTQueueIO

trySubmitFlakeToRepo :: PoF m => IpAdr -> FlakeRepo -> FlakeUrl -> m (Either Text Flake)
trySubmitFlakeToRepo ip fr fu = do
  now <- liftIO getCurrentTime
  atomicalog $ do
    lift (lookup fu fr.flakes) >>= \case
      Nothing -> submitFlakeToRepo now
      Just bf@BadFlake {}
        | now `diffUTCTime` bf.fetcherRespondedAt > untag fr.wsArgs.allowResubmitBadFlakeIn -> do
            $(logInfo) $ "Resubmit flake " <> P.show fu
            submitFlakeToRepo now
        | otherwise ->
          pure . Left $ "Flake resubmitted within " <> P.show (untag fr.wsArgs.allowResubmitBadFlakeIn)
      Just f ->
        pure $ Right f
  where
    submitFlakeToRepo now = do
      ql <- lift (readTVar fr.fetcherQueueLen)
      if ql > 1000
        then pure $ Left "Submition Queue is full"
        else do
          lift $ do
            modifyTVar' fr.repoStats.submittedFlakes (1 +)
            modifyTVar' fr.repoStats.totalFlakeUploadsSinceRestart (1 +)
            modifyTVar' fr.fetcherQueueLen (1 +)
            writeTQueue fr.fetcherQueue $ Just fu
          fql <- lift $ readTVar fr.fetcherQueueLen
          $(logInfo) $ "Fetcher queue increased to " <> P.show fql
          let f = SubmittedFlake fu now ip in do
            lift $ insert f fu fr.flakes
            pure $ Right f

popFlakeSubmition :: PoF m => FlakeRepo -> FetcherId -> m (Maybe FlakeUrl)
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
            lift $ do
              modifyTVar' fr.repoStats.submittedFlakes (flip (-) 1)
              modifyTVar' fr.repoStats.fetchingFlakes (1 +)
              insert (FlakeIsBeingFetched flakeUrl now ftid) fu fr.flakes
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
addFetchedFlake :: PoF m =>
  FlakeRepo ->
  FetcherId ->
  (FlakeUrl, Either Text MetaFlake) ->
  m (Maybe FlakeUrl)
addFetchedFlake fr ftid (fu, fetchedFlake) = do
  now <- liftIO getCurrentTime
  atomicalog (go now) >> atomicalog (popFlakeSubmitionStm ftid fr now)
  where
    go now = do
      lift (lookup fu fr.flakes) >>= \case
        Just (FlakeIsBeingFetched exFu past _fid)
          | fu == exFu ->
            case fetchedFlake of
              Left e -> do
                lift $ do
                  modifyTVar' fr.repoStats.fetchingFlakes (flip (-) 1)
                  modifyTVar' fr.repoStats.badFlakes (1 +)
                  insert (BadFlake fu now e) fu fr.flakes
                $(logInfo) $ "Fetching flake " <> P.show fu <> " failed in " <> P.show (duration now past)

              Right meta -> do
                let f = FlakeFetched fu now meta
                lift $ do
                  modifyTVar' fr.repoStats.fetchingFlakes (flip (-) 1)
                  modifyTVar' fr.repoStats.fetchedFlakes (1 +)
                  insert f fu fr.flakes
                $(logInfo) $ "Flake " <> P.show fu <> " is fetched in " <> P.show (duration now past)
                lift $ writeTQueue fr.acidQueue (fu, f)
                indexNewFlake fr.flakeIndex fu
          | otherwise ->
              $(logError) $ "Error flake url mismatch " <> P.show fu <> " <> " <> P.show exFu
        Just ufs ->
          $(logError) $ "Expected FlakeIsBeingFetched state but: " <> P.show ufs
        Nothing ->
          $(logError) $ "Error flake " <> P.show fu <> " is missing in map"

sendEmtpyFlakeSubmition ::
  PoF m => FlakeRepo -> Tagged NoSubmitionHeartbeatSec Second -> m ()
sendEmtpyFlakeSubmition fr (Tagged d) = do
  liftIO $ threadDelay $ fromIntegral (convertUnit d :: Microsecond)
  atomicalog $ do
    fql <- lift $ readTVar fr.fetcherQueueLen
    when (fql == 0) $ do
      $(logInfo) "Send empty Flake Submition"
      lift $ do
        writeTQueue fr.fetcherQueue Nothing
        modifyTVar' fr.fetcherQueueLen (1 +)

validateRawFlakeUrl :: RawFlakeUrl -> Maybe FlakeUrl
validateRawFlakeUrl (RawFlakeUrl rfu) =
  if rfu =~ ("^github:[a-zA-Z0-9._-]+[/][a-zA-Z0-9._-]+$" :: Text)
  then pure $ FlakeUrl rfu
  else Nothing
