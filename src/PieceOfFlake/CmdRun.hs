module PieceOfFlake.CmdRun where

import Data.Version (showVersion)
import Data.ByteString.Char8 qualified as S8
import Paths_a_piece_of_flake ( version )
import PieceOfFlake.Acid ( loadFromDb, openFlakeDb, runPersistQueue )
import PieceOfFlake.CmdArgs
    ( WsCmdArgs(staticCache, indexQueryCacheSize, acidFlakes,
                ringBufferSize, logLevel, fetcherSecretPath, noSubmitionHeartbeat, fetcherHeartbeatPeriod),
      CmdArgs(..),
      FetcherCmdArgs(fetcherSecretPath, FetcherCmdArgs, logLevel),
      SubmitListOfFlakesArgs(logLevel) )
import PieceOfFlake.Fetcher ( runFetcher )
import PieceOfFlake.Flake.Repo
    ( FlakeRepo(flakeIndex, wsArgs, acidFlakes, acidQueue, repoStats,
                flakes),
      mkFlakeRepo,
      removeOldBadFlakes,
      sendEmptyFlakeSubmition,
      resubmitFlakesFetchingByZombie )
import PieceOfFlake.Http ( runWebService )
import PieceOfFlake.Index
    ( mkFlakeIndex, loadIndexFromScratch, consumeIndexQueue )
import PieceOfFlake.Page ( Ypp(Ypp) )
import PieceOfFlake.Prelude
import PieceOfFlake.Stats ( mkRepoStats )
import PieceOfFlake.SubmitList ( runSubmitList )
import PieceOfFlake.WebService
    ( FetcherSecret(..), NoSubmitionHeartbeatSec )
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
  forkForever "Lost flake resubmition" $ do
    resubmitFlakesFetchingByZombie fr
    threadDelay . (* 3) $ untag fr.wsArgs.fetcherHeartbeatPeriod
  forkForever "Flake persistence" $ do
    recoverAll
      (fibonacciBackoff 10000)
      (\_ -> runPersistQueue fr.acidFlakes fr.acidQueue)
  forkForever "Flake text search indexer"
    $ consumeIndexQueue fr.repoStats fr.flakes fr.flakeIndex
  forkForever "Empty Submition" $ do
    threadDelay $ untag period
    sendEmptyFlakeSubmition fr
  forkForever "Old Bad Flake Collector" $ removeOldBadFlakes fr
  where
    forkForever thrName f = do
      tid <- forkFinally
        (forever f)
        (\case
            Left e -> $(logError) $ thrName <> " thread ended: " <> show e
            Right () -> $(logInfo) $ thrName <> " thread ended without errors")
      $(logInfo) $ thrName <> " thread is forked with tid: " <> show tid

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
      runWebService ws $ Ypp fr ws.staticCache
  FetcherJob fa@FetcherCmdArgs {} ->
      withLogs fa.logLevel $ do
        $(logInfo) $ "Start Fetcher "  <> show fa
        runFetcher fa =<< loadFetcherSecret fa.fetcherSecretPath
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
