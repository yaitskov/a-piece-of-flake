{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.CmdRun where

import Data.Version (showVersion)
import Language.Haskell.TH.Syntax (qLocation)
import PieceOfFlake.Acid
    ( loadFromDb, openFlakeDb, runPersistQueue )
import PieceOfFlake.CmdArgs
import PieceOfFlake.Fetcher ( runFetcher )
import PieceOfFlake.Flake.Repo
    ( FlakeRepo(indexerQueueLen, flakeIndex, flakes, acidFlakes,
                acidQueue, indexerQueue),
      mkFlakeRepo,
      sendEmtpyFlakeSubmition )
import PieceOfFlake.Index
    ( consumeIndexQueue, emptyFlakeIndex, loadIndexFromScratch )
import PieceOfFlake.Page ( Ypp(Ypp) )
import PieceOfFlake.Prelude
import PieceOfFlake.Req ( setResponseTimeout )
import Network.Wai.Handler.WarpTLS ( runTLS, tlsSettings, TLSSettings )
import Network.Wai.Handler.Warp
    ( Settings,
      setBeforeMainLoop,
      setMaxTotalHeaderLength,
      setOnException,
      setPort,
      setServerName,
      setSlowlorisSize,
      setTimeout,
      runSettings,
      defaultSettings,
      defaultShouldDisplayException )

import Paths_a_piece_of_flake ( version )
import Yesod.Core
    ( Yesod(makeLogger, messageLoggerSource), Application, toWaiApp )

import StmContainers.Map ( newIO )
import UnliftIO.Concurrent ( forkFinally )
import UnliftIO.Retry ( fibonacciBackoff, recoverAll )
import Yesod.Core.Types ( Logger )

mkSettings :: Ypp -> WsCmdArgs -> Logger -> Settings
mkSettings yp ca logger =
  setPort port $
  setServerName "PieceOfFlake" $
  setOnException onEx $
  setSlowlorisSize 1024 $
  setMaxTotalHeaderLength 1024 $
  setBeforeMainLoop  (putTextLn $ "Go " <> untag ca.baseUrl) $
  -- expected that Nginx restricts keepalive for non fetcher connections
  setTimeout (2 * fromIntegral (untag ca.noSubmitionHeartbeat))
  defaultSettings

  where
    port = untag ca.httpPortToListen
    shouldLog' = defaultShouldDisplayException
    onEx _ e =
      when (shouldLog' e) $
      messageLoggerSource
      yp
      logger
      $(qLocation >>= liftLoc)
      "yesod-core"
      LevelError
      (toLogStr $ "Exception from Warp: " ++ show e)

mkTlsSettings :: Tagged Cert FilePath -> Tagged CertKey FilePath -> TLSSettings
mkTlsSettings cert key = tlsSettings (untag cert) (untag key)

runPlain :: Settings -> Application -> IO ()
runPlain = runSettings

initRepo :: PoF m => FetcherSecret -> WsCmdArgs -> m FlakeRepo
initRepo fsec wsa = do
  flakesMap <- liftIO newIO
  fi <- newTVarIO emptyFlakeIndex
  acidFlakeStorage <- openFlakeDb wsa.acidFlakes
  loadIndexFromScratch fi flakesMap . reverse =<< loadFromDb acidFlakeStorage
  mkFlakeRepo fsec wsa fi flakesMap acidFlakeStorage

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
    (forever $ consumeIndexQueue fr.flakes fr.indexerQueue fr.indexerQueueLen fr.flakeIndex)
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

      let y = Ypp fr ws.staticCache ws.baseUrl
      lift $ do
        logger <- makeLogger y
        case liftA2 mkTlsSettings ws.certFile ws.keyFile of
          Nothing -> runPlain (mkSettings y ws logger) =<< toWaiApp y
          Just tlsSngs -> runTLS tlsSngs (mkSettings y ws logger) =<< toWaiApp y
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
