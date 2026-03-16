module PieceOfFlake.Flake.Repo where

import Data.Acid ( AcidState )
import ListT qualified as L
import PieceOfFlake.Acid ( AcidFlakes )
import PieceOfFlake.Flake
import PieceOfFlake.CmdArgs
import PieceOfFlake.Index ( FlakeIndex, indexNewFlake )
import PieceOfFlake.Prelude hiding (Map, show)
import PieceOfFlake.Prelude qualified as P
import PieceOfFlake.Stats
    ( RepoStatsF(totalFlakeUploadsSinceRestart, badFlakes,
                 fetchingFlakes, fetchedFlakes, meanFetchTime, submittedFlakes, meanTimeInFetchQueue),
      addTimeDif )
import PieceOfFlake.Stm ( newTQueueIO, readTQueue, writeTQueue, TQueue, atomicalog )
import PieceOfFlake.WebService

import StmContainers.Map
    ( Map, delete, insert, listTNonAtomic, lookup, newIO )
import Text.Regex.TDFA ( (=~) )

data FetcherState
  = FetcherState
  { workingOn :: Maybe FlakeUrl
  , lastHeartbeatAt :: UtcBox
  }

data FlakeRepo
  = FlakeRepo
  { flakes :: Map FlakeUrl Flake
  , fetcherSecret :: FetcherSecret
  , flakeIndex :: FlakeIndex
  , wsArgs :: WsCmdArgs
  , acidFlakes :: AcidState AcidFlakes
  , repoStats :: RepoStatsF TVar
  , fetchers :: Map FetcherId FetcherState
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
  atomicalog $ do
    lift (lookup fu fr.flakes) >>= \case
      Nothing -> submitFlakeToRepo . mkUtcBox =<< getCurrentTime
      Just bf@BadFlake {} ->
        doAfter bf.fetcherRespondedAt
        (\fra -> do
            now <- lift $ getTimeAfter fra
            if now `diffUTCTime` fra > untag fr.wsArgs.allowResubmitBadFlakeIn then do
              $(logInfo) $ "Resubmit bad flake " <> P.show fu
              submitFlakeToRepo $ mkUtcBox now
            else
              pure . Left $ "Flake resubmitted within " <> P.show (untag fr.wsArgs.allowResubmitBadFlakeIn))
      Just fi@FlakeIndexed {} ->
        doAfter fi.indexedAt
        (\fra -> do
            now <- lift $ getTimeAfter fra
            if now `diffUTCTime` fra > untag fr.wsArgs.allowResubmitIndexedFlakeIn then do
              $(logInfo) $ "Resubmit indexed flake " <> P.show fu
              submitFlakeToRepo $ mkUtcBox now
            else
              pure . Left $ "Indexed flake resubmitted within " <> P.show (untag fr.wsArgs.allowResubmitIndexedFlakeIn))
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
  atomicalog (popFlakeSubmitionStm ftid fr) >>= mapM
    (\(fu, Tagged ifq) -> do
       addTimeDif fr.repoStats.meanTimeInFetchQueue ifq
       pure fu)

data TimeInFetchQueue

popFlakeSubmitionStm  ::
  FetcherId ->
  FlakeRepo ->
  WriterLoggingT STM (Maybe (FlakeUrl, Tagged TimeInFetchQueue NominalDiffTime))
popFlakeSubmitionStm ftid fr =
  lift (lookup ftid fr.fetchers) >>= \case
    Nothing -> do
      $(logDebug) $ "Fetcher " <> P.show ftid <> " asks for flake url from scratch"
      fromScratch
    Just FetcherState { workingOn = Nothing } -> do
      $(logDebug) $ "Fetcher " <> P.show ftid <> " asks for flake url from scratch2"
      fromScratch
    Just FetcherState { workingOn = Just lostFu } -> do
      $(logWarn) $ "Fetcher " <> P.show ftid <> " asked for a next flake url, but "
        <> "has not responded about " <> P.show lostFu
      fetcherResume lostFu
  where
    fromScratch = do
      fSub <- lift $ readTQueue fr.fetcherQueue
      lift $ modifyTVar' fr.fetcherQueueLen (\x -> x - 1)
      fql <- lift $ readTVar fr.fetcherQueueLen
      $(logInfo) $ "Fetcher queue decreased to " <> P.show fql
      case fSub of
        Nothing -> pure Nothing
        Just fu -> resumeWithFu fu
    fetcherResume fu = do
      lift (lookup fu fr.flakes) >>= \case
        Just (FlakeIsBeingFetched { flakeUrl, submitionFetchedAt })
          | flakeUrl == fu -> do
              doAfter submitionFetchedAt (\sa -> do
                now <- getTimeAfter sa
                pure $ Just (flakeUrl, Tagged $ now `diffUTCTime` sa))
          | otherwise -> do
            $(logError) $ "Error flake url mismatch " <> P.show fu <> " <> " <> P.show flakeUrl
            popFlakeSubmitionStm ftid fr
        Just ufs -> do
          $(logError) $ "Expected SumbittedFlake state but:" <> P.show ufs
          popFlakeSubmitionStm ftid fr
        Nothing -> do
          $(logError) $ "Error flake " <> P.show fu <> " is missing in map"
          popFlakeSubmitionStm ftid fr

    resumeWithFu fu = do
      lift (lookup fu fr.flakes) >>= \case
        Just (SubmittedFlake { flakeUrl, submittedAt })
          | flakeUrl == fu -> do
              lift $ do
                modifyTVar' fr.repoStats.submittedFlakes (flip (-) 1)
                modifyTVar' fr.repoStats.fetchingFlakes (1 +)

              doAfter submittedAt (\sa -> do
                now <- getTimeAfter sa
                assocFlakeWithFetcher now flakeUrl ftid fr.fetchers fr.flakes
                lift $ insert (FlakeIsBeingFetched flakeUrl (mkUtcBox now) ftid) fu fr.flakes
                pure $ Just (flakeUrl, Tagged $ now `diffUTCTime` sa))
          | otherwise -> do
            $(logError) $ "Error flake url mismatch " <> P.show fu <> " <> " <> P.show flakeUrl
            popFlakeSubmitionStm ftid fr
        Just ufs -> do
          $(logError) $ "Expected SumbittedFlake state but:" <> P.show ufs
          popFlakeSubmitionStm ftid fr
        Nothing -> do
          $(logError) $ "Error flake " <> P.show fu <> " is missing in map"
          popFlakeSubmitionStm ftid fr

deassocFlakeFromFetcher ::
  (MonadTrans t, Hashable key, MonadLogger (t STM), Show key) =>
  UTCTime n -> FlakeUrl -> key -> Map key FetcherState -> t STM ()
deassocFlakeFromFetcher now fu ftid fetchers = do
  lift (lookup ftid fetchers) >>= \case
    Nothing -> do
      $(logWarn) $ "Fetcher " <> P.show ftid <> " has no state"
      lift $ insert efs ftid fetchers
    Just FetcherState { workingOn = Nothing } ->
      $(logWarn) $ "Fetcher " <> P.show ftid <> " was not associated with any flake"
    Just FetcherState { workingOn = Just afu }
      | afu == fu -> do
         lift $ insert efs ftid fetchers
         $(logDebug) $ "Fetcher " <> P.show ftid <> " is diassociated from flake " <> P.show afu
      | otherwise -> do
         $(logWarn) $ "Fetcher " <> P.show ftid <> " was diassociated with flake " <> P.show afu
           <> " but returned " <> P.show fu
         lift $ insert efs ftid fetchers
  where
    efs = FetcherState { workingOn = Nothing, lastHeartbeatAt = mkUtcBox now }

assocFlakeWithFetcher :: (MonadTrans t, Hashable a, MonadLogger (t STM), Show a) =>
  UTCTime n -> FlakeUrl -> a -> Map a FetcherState -> Map FlakeUrl Flake -> t STM ()
assocFlakeWithFetcher now fu ftid fetchers flakes = do
  lift (lookup ftid fetchers) >>= \case
    Nothing -> do
      $(logDebug) $ "Fetcher " <> P.show ftid <> " got flake " <> P.show fu
      lift $ insert FetcherState { workingOn = Just fu, lastHeartbeatAt = mkUtcBox now } ftid fetchers
    Just FetcherState { workingOn = Nothing } -> do
      $(logDebug) $ "Fetcher " <> P.show ftid <> " got flake' " <> P.show fu
      lift $ insert FetcherState { workingOn = Just fu, lastHeartbeatAt = mkUtcBox now } ftid fetchers
    Just FetcherState { workingOn = Just lostFu }
      | lostFu == fu ->
          $(logDebug) $ "Fetcher " <> P.show ftid <> " resumes on " <> P.show fu
      | otherwise -> do
          lift $ insert FetcherState { workingOn = Just fu, lastHeartbeatAt = mkUtcBox now } ftid fetchers
          lift (lookup lostFu flakes) >>= \case
            Nothing ->
              $(logWarn) $ "Fetcher " <> P.show ftid <>
                " lost a flake that is missing in the flake map " <> P.show lostFu
            Just fbf@FlakeIsBeingFetched {}
              | lostFu == fbf.flakeUrl -> do
                  $(logWarn) $ "Flake " <> P.show lostFu <> " has been lost on fetcher " <> P.show ftid
                  lift $ insert (BadFlake lostFu (mkUtcBox now) "Flake has been lost on fetcher. Try resubmit.")
                           lostFu flakes
              | otherwise -> do
                  $(logError) $ "Lost flake url " <> P.show lostFu <> " mismatch with url in map " <> P.show fbf
            Just ue ->
              $(logWarn) $ "Flake " <> P.show fu <> " is bound to fetcher " <>
                P.show ftid <> " with strange state " <> P.show ue

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
  mapM_ (addTimeDif fr.repoStats.meanFetchTime) =<< atomicalog go
  atomicalog (popFlakeSubmitionStm ftid fr) >>= mapM
    (\(fu', Tagged sa) -> do
       addTimeDif fr.repoStats.meanTimeInFetchQueue sa
       pure fu')
  where
    go = do
      lift (lookup fu fr.flakes) >>= \case
        Just (FlakeIsBeingFetched exFu past _fid)
          | fu == exFu ->
            doAfter past $ \p -> do
              now <- lift $ getTimeAfter p
              deassocFlakeFromFetcher now fu ftid fr.fetchers
              case fetchedFlake of
                Left e -> do
                  lift $ do
                    modifyTVar' fr.repoStats.fetchingFlakes (flip (-) 1)
                    modifyTVar' fr.repoStats.badFlakes (1 +)
                    insert (BadFlake fu (mkUtcBox now) e) fu fr.flakes
                  $(logInfo) $ "Fetching flake " <> P.show fu <> " failed in " <> P.show (now `diffUTCTime` p)
                  pure Nothing
                Right meta -> do
                  let f = FlakeFetched fu (mkUtcBox now) meta
                  lift $ do
                    modifyTVar' fr.repoStats.fetchingFlakes (flip (-) 1)
                    modifyTVar' fr.repoStats.fetchedFlakes (1 +)
                    insert f fu fr.flakes
                  let fetchTime = now `diffUTCTime` p
                  $(logInfo) $ "Flake " <> P.show fu <> " is fetched in " <> P.show fetchTime
                  lift $ writeTQueue fr.acidQueue (fu, f)
                  indexNewFlake fr.flakeIndex fu
                  pure $ Just fetchTime
          | otherwise -> do
              $(logError) $ "Error flake url mismatch " <> P.show fu <> " <> " <> P.show exFu
              pure Nothing
        Just ufs -> do
          $(logError) $ "Expected FlakeIsBeingFetched state but: " <> P.show ufs
          pure Nothing
        Nothing -> do
          $(logError) $ "Error flake " <> P.show fu <> " is missing in map"
          pure Nothing

sendEmptyFlakeSubmition ::
  PoF m => FlakeRepo -> Tagged NoSubmitionHeartbeatSec Second -> m ()
sendEmptyFlakeSubmition fr (Tagged d) = do
  threadDelay d
  atomicalog $ do
    fql <- lift $ readTVar fr.fetcherQueueLen
    when (fql == 0) $ do
      $(logInfo) "Send empty Flake Submition"
      lift $ do
        writeTQueue fr.fetcherQueue Nothing
        modifyTVar' fr.fetcherQueueLen (1 +)

selectBadOldFlakes :: MonadIO m => FlakeRepo -> m [ FlakeUrl ]
selectBadOldFlakes fr =
  liftIO (L.foldMaybe filterBad [] (listTNonAtomic fr.flakes))
  where
    filterBad selected = \case
      (_, BadFlake { flakeUrl }) -> pure . Just $ flakeUrl : selected
      _ -> pure Nothing

removeOldBadFlakes :: PoF m => FlakeRepo -> m ()
removeOldBadFlakes fr  = do
  threadDelay $ untag fr.wsArgs.badFlakeMaxAge `div` 2
  badFlakes <- selectBadOldFlakes fr
  forM_ badFlakes $ \fu ->
    atomicalog $ do
      lift (lookup fu fr.flakes) >>= \case
        Just bf@BadFlake {} ->
          doAfter bf.fetcherRespondedAt $ \fra -> do
             now <- getTimeAfter fra
             when (now `diffUTCTime` fra > toNominal (untag fr.wsArgs.badFlakeMaxAge)) $ do
               $(logInfo) $ "Delete old bad flake " <> P.show fu
               lift $ delete fu fr.flakes
        _ -> pure ()

validateRawFlakeUrl :: RawFlakeUrl -> Maybe FlakeUrl
validateRawFlakeUrl (RawFlakeUrl rfu) =
  if rfu =~ ("^github:[a-zA-Z0-9._-]+[/][a-zA-Z0-9._-]+$" :: Text)
  then pure $ FlakeUrl rfu
  else Nothing

fetcherIsAlive :: PoF m => FlakeRepo -> FetcherHeartbeat -> m ()
fetcherIsAlive fr fhb =
  atomicalog $ do
    now <- getCurrentTime
    lift $ do
      lookup fhb.fetcherId fr.fetchers >>= \case
        Nothing ->
          insert
            FetcherState { workingOn = Nothing
                         , lastHeartbeatAt = mkUtcBox now
                         }
            fhb.fetcherId fr.fetchers
        Just ftst ->
          insert ftst { lastHeartbeatAt = mkUtcBox now } fhb.fetcherId fr.fetchers

resubmitFlakesFetchingByZombie :: PoF m => FlakeRepo -> m ()
resubmitFlakesFetchingByZombie fr = do
  fs <- liftIO (L.toList (listTNonAtomic fr.fetchers))
  atomicalog $ do
    forM_ fs $ \(ftid, ftst) -> do
      case ftst.workingOn of
        Nothing -> do
          $(logInfo) $ "Delete state of "  <> P.show ftid <> " fetcher because of idling"
          lift $ delete ftid fr.fetchers
        Just wasWorkingOnFu -> do
          doAfter ftst.lastHeartbeatAt $ \ha -> do
            now <- getTimeAfter ha
            let maxPeriod = (*2) . toNominal $ untag fr.wsArgs.fetcherHeartbeatPeriod
            when (now `diffUTCTime` ha > maxPeriod) $ do
              $(logWarn) $ "Fetcher " <> P.show ftid <> " must be dead since " <> P.show now
              lift $ delete ftid fr.fetchers
              lift (lookup wasWorkingOnFu fr.flakes) >>= \case
                Nothing ->
                  $(logError) $ "Fetcher " <> P.show ftid <> " was working an ghost flake: " <> P.show wasWorkingOnFu
                Just fif@FlakeIsBeingFetched {}
                  | fif.fetcherId /= ftid ->
                      $(logError) $ "Failed to resubmit flake " <> P.show wasWorkingOnFu <>
                        " due it is processed by another fetcher " <> P.show fif.fetcherId
                  | fif.flakeUrl /= wasWorkingOnFu -> do
                      $(logWarn) $ "Flake map is corrupted on" <>
                        P.show wasWorkingOnFu <> " /= " <> P.show fif.flakeUrl
                  | otherwise -> do
                      $(logInfo) $ "Resubmit flake " <> P.show wasWorkingOnFu <>
                        " after fetcher " <> P.show ftid <> " died"
                      lift $ do
                        insert
                          SubmittedFlake
                            { flakeUrl = wasWorkingOnFu
                            , submittedAt = mkUtcBox now
                            , submittedFrom = IpAdr "127.0.0.1"
                            }
                          wasWorkingOnFu fr.flakes
                        modifyTVar' fr.fetcherQueueLen (1 +)
                        writeTQueue fr.fetcherQueue $ Just wasWorkingOnFu
                Just o ->
                  $(logError) $ "Failed to resubmit flake " <> P.show wasWorkingOnFu <>
                      " due it is not in IsBeingFetching state but " <> P.show o
