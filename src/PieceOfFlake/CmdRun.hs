module PieceOfFlake.CmdRun where

import Data.Version (showVersion)

import PieceOfFlake.Acid
    ( loadFromDb, openFlakeDb, runPersistQueue )
import PieceOfFlake.CmdArgs
import PieceOfFlake.Fetcher ( runFetcher )
import PieceOfFlake.Flake.Repo
    ( FlakeRepo(flakeIndex, acidFlakes, acidQueue, repoStats, flakes),
      mkFlakeRepo,
      sendEmtpyFlakeSubmition )
import PieceOfFlake.Http
import PieceOfFlake.Index
    ( mkFlakeIndex, loadIndexFromScratch, consumeIndexQueue )
import PieceOfFlake.Page ( Ypp(Ypp) )
import PieceOfFlake.Prelude
import PieceOfFlake.Req ( setResponseTimeout )
import PieceOfFlake.Stats ( mkRepoStats )
import Paths_a_piece_of_flake ( version )
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
    (forever $ sendEmtpyFlakeSubmition fr period)
    (\case
        Left e -> $(logError) $ "Empty Submition Thead ended: " <> show e
        Right () -> $(logInfo) "Empty Submition Thead ended without errors")
  $(logInfo) $ "Empty Submition thread is forked " <> show efsTid


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

  PieceOfFlakeVersion ->
    putStrLn $ "Version " <> showVersion version

withLogs :: MonadIO m => LogLevel -> LoggingT m a -> m a
withLogs minLogL a =
  runStdoutLoggingT $ filterLogger (\_ l -> l >= minLogL) a

loadFetcherSecret :: MonadIO m => Tagged FetcherSecret FilePath -> m FetcherSecret
loadFetcherSecret a = readFileTxt (untag a) <&> FetcherSecret
