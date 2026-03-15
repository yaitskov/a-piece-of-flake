module PieceOfFlake.CmdRun where

import Data.Version (showVersion)
import Data.ByteString.Char8 qualified as S8
import Paths_a_piece_of_flake ( version )
import PieceOfFlake.Acid ( loadFromDb, openFlakeDb, runPersistQueue )
import PieceOfFlake.CmdArgs
    ( FetcherSecret(..),
      WsCmdArgs(baseUrl, indexQueryCacheSize, acidFlakes, ringBufferSize,
                logLevel, fetcherSecretPath, noSubmitionHeartbeat, staticCache),
      NoSubmitionHeartbeatSec,
      CmdArgs(..),
      FetcherCmdArgs(fetcherSecretPath, FetcherCmdArgs, webServiceUrl,
                     noSubmitionHeartbeat, logLevel),
      SubmitListOfFlakesArgs(logLevel) )
import PieceOfFlake.Fetcher ( runFetcher )
import PieceOfFlake.Flake.Repo
    ( FlakeRepo(flakeIndex, acidFlakes, acidQueue, repoStats, flakes),
      mkFlakeRepo, removeOldBadFlakes,
      sendEmptyFlakeSubmition )
import PieceOfFlake.Http ( runWebService )
import PieceOfFlake.Index
    ( mkFlakeIndex, loadIndexFromScratch, consumeIndexQueue )
import PieceOfFlake.Page ( Ypp(Ypp) )
import PieceOfFlake.Prelude
import PieceOfFlake.Req ( setResponseTimeout )
import PieceOfFlake.Stats ( mkRepoStats )
import PieceOfFlake.SubmitList ( runSubmitList )
import StmContainers.Map ( newIO )
import UnliftIO.Concurrent ( forkFinally )
import UnliftIO.Retry ( fibonacciBackoff, recoverAll )

initRepo :: PoF m => FetcherSecret -> WsCmdArgs -> m FlakeRepo
initRepo fsec wsa = do
  flakesMap <- liftIO newIO
  fi <- mkFlakeIndex wsa.indexQueryCacheSize
  acidFlakeStorage <- openFlakeDb wsa.acidFlakes
  rs <- mkRepoStats wsa.ringBufferSize
  loadIndexFromScratch rs fi flakesMap . reverse =<< loadFromDb acidFlakeStorage
  mkFlakeRepo fsec wsa fi flakesMap acidFlakeStorage rs

launchBackgroundThreads :: PoF m =>
  Tagged NoSubmitionHeartbeatSec Second -> FlakeRepo -> m ()
launchBackgroundThreads period fr = do
  persisFlakeTid <- forkFinally
    (forever $ do
      recoverAll
        (fibonacciBackoff 10000)
        (\_ -> runPersistQueue fr.acidFlakes fr.acidQueue))
    (\case
        Left e -> $(logError) $ "Flake Persistence thread ended: " <> show e
        Right () -> $(logInfo) "Flake Persistence thread ended without errors")
  $(logInfo) $ "Flake persistence thread is forked " <> show persisFlakeTid
  idxFlakeTid <- forkFinally
    (forever $ consumeIndexQueue fr.repoStats fr.flakes fr.flakeIndex)
    (\case
        Left e -> $(logError) $ "Flake text search indexer thread ended: " <> show e
        Right () -> $(logInfo) "Flake text search indexer thread without errors")
  $(logInfo) $ "Flake text search indexer thread is forked " <> show idxFlakeTid
  efsTid <- forkFinally
    (forever $ sendEmptyFlakeSubmition fr period)
    (\case
        Left e -> $(logError) $ "Empty Submition thread ended: " <> show e
        Right () -> $(logInfo) "Empty Submition thread ended without errors")
  $(logInfo) $ "Empty Submition thread is forked " <> show efsTid
  obfTid <- forkFinally
    (forever $ removeOldBadFlakes fr)
    (\case
        Left e -> $(logError) $ "Old Bad Flake Collector thread ended: " <> show e
        Right () -> $(logInfo) "Old Bad Flake Collector thread ended without errors")
  $(logInfo) $ "Old Bad Flake Collector thread is forked " <> show obfTid

-- commented lines below are excuted via: @ghciwatch --enable-eval@
-- $> import PieceOfFlake.CmdArgs
-- $> execWithArgs runCmd . (:[]) =<< (fromMaybe "web" <$> PieceOfFlake.Prelude.lookupEnv "E")
runCmd :: CmdArgs -> IO ()
runCmd = \case
  WebService ws ->
    withLogs ws.logLevel $ do
      $(logInfo) $ "Start WebService "  <> show ws
      fr <- (`initRepo` ws) =<< loadFetcherSecret ws.fetcherSecretPath
      launchBackgroundThreads ws.noSubmitionHeartbeat fr
      runWebService ws $ Ypp fr ws.staticCache ws.baseUrl
  FetcherJob fa@FetcherCmdArgs {} -> -- serUrl rawNixCache fid fSecPath reqTimeout miLogLevel) ->
    let serUrl' = (setResponseTimeout fa.webServiceUrl $ untag fa.noSubmitionHeartbeat) in -- reqTimeout) in
      withLogs fa.logLevel $ do
        $(logInfo) $ "Start Fetcher "  <> show fa
        runFetcher serUrl' fa =<< loadFetcherSecret fa.fetcherSecretPath
  SubmitListOfFlakes sla ->
    withLogs sla.logLevel $ runSubmitList sla
  PieceOfFlakeVersion ->
    putStrLn $ "Version " <> showVersion version

withLogs :: MonadIO m => LogLevel -> LoggingT m a -> m a
withLogs minLogL m =
  runFlushingStdoutLoggingT $ filterLogger (\_ l -> l >= minLogL) m
  where
    runFlushingStdoutLoggingT = (`runLoggingT` flushingOutput stdout)
      where
        -- without explicit flushing - syslog does to get logs
        flushingOutput h loc src level msg = S8.hPutStr h ls >> hFlush h
          where
            ls = defaultLogStrBS loc src level msg
            defaultLogStrBS a b c d = fromLogStr $ defaultLogStr a b c d


loadFetcherSecret :: MonadIO m => Tagged FetcherSecret FilePath -> m FetcherSecret
loadFetcherSecret a = readFileTxt (untag a) <&> FetcherSecret
