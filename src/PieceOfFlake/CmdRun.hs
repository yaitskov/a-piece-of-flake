{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.CmdRun where

import Control.Monad.Logger ( liftLoc, ToLogStr(toLogStr) )
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
    ( toWaiApp,
      LogLevel(LevelError),
      Application,
      Yesod(makeLogger, messageLoggerSource) )
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

initRepo :: MonadIO m => FetcherSecret -> Tagged AcidFlakesPath FilePath -> m FlakeRepo
initRepo fsec acidFlakes = do
  flakesMap <- liftIO newIO
  fi <- newTVarIO emptyFlakeIndex
  acidFlakeStorage <- openFlakeDb acidFlakes
  loadIndexFromScratch fi flakesMap . reverse =<< loadFromDb acidFlakeStorage
  mkFlakeRepo fsec fi flakesMap acidFlakeStorage

launchBackgroundThreads :: MonadUnliftIO m => FlakeRepo -> m ()
launchBackgroundThreads fr = do
  persisFlakeTid <- forkFinally
    (forever $ do
      recoverAll
        (fibonacciBackoff 10000)
        (\_ -> runPersistQueue fr.acidFlakes fr.acidQueue))
    (\case
        Left e -> putStrLn $ "Flake Persistence thread ended: " <> show e
        Right () -> putStrLn "Flake Persistence thread ended without errors")
  putStrLn $ "Flake persistence thread is forked " <> show persisFlakeTid
  idxFlakeTid <- forkFinally
    (forever $ consumeIndexQueue fr.flakes fr.indexerQueue fr.indexerQueueLen fr.flakeIndex)
    (\case
        Left e -> putStrLn $ "Flake text search indexer thread ended: " <> show e
        Right () -> putStrLn "Flake text search indexer thread without errors")
  putStrLn $ "Flake text search indexer thread is forked " <> show idxFlakeTid
  efsTid <- forkFinally (sendEmtpyFlakeSubmition fr 30_000_000)
    (\case
        Left e -> putStrLn $ "Empty Submition Thead ended: " <> show e
        Right () -> putStrLn "Empty Submition Thead ended without errors")
  putStrLn $ "Empty Submition thread is forked " <> show efsTid

-- commented lines below are excuted via: @ghciwatch --enable-eval@
-- $> import PieceOfFlake.CmdArgs
-- $> execWithArgs runCmd ["web"]
runCmd :: CmdArgs -> IO ()
runCmd = \case
  WebService ws -> do
    $(trIo "start/ws")
    fr <- (`initRepo` ws.acidFlakes) =<< loadFetcherSecret ws.fetcherSecretPath
    launchBackgroundThreads fr

    let y = Ypp fr ws.staticCache ws.baseUrl

    logger <- makeLogger y
    case liftA2 mkTlsSettings ws.certFile ws.keyFile of
      Nothing -> runPlain (mkSettings y ws logger) =<< toWaiApp y
      Just tlsSngs -> runTLS tlsSngs (mkSettings y ws logger) =<< toWaiApp y
  FetcherJob (FetcherCmdArgs serUrl rawNixCache fid fSecPath) ->
    runFetcher serUrl rawNixCache fid =<< loadFetcherSecret fSecPath
  PieceOfFlakeVersion ->
    putStrLn $ "Version " <> showVersion version

loadFetcherSecret :: MonadIO m => Tagged FetcherSecret FilePath -> m FetcherSecret
loadFetcherSecret a = readFileTxt (untag a) <&> FetcherSecret
