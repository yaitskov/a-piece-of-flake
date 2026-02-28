{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.CmdRun where

import Control.Monad.Logger ( liftLoc, ToLogStr(toLogStr) )
import Data.Version (showVersion)
import Language.Haskell.TH.Syntax (qLocation)
import PieceOfFlake.CmdArgs ( CmdArgs(..), CertKey, Cert )
import PieceOfFlake.Fetcher ( runFetcher )
import PieceOfFlake.Flake
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
import Yesod.Core.Types ( Logger )
import UnliftIO.Concurrent

mkSettings :: Ypp -> CmdArgs -> Logger -> Settings
mkSettings yp ca logger =
  setPort port $
  setServerName "PieceOfFlake" $
  setOnException onEx $
  setSlowlorisSize 1024 $
  setMaxTotalHeaderLength 1024 $
  setBeforeMainLoop
  (putStrLn $ "Go http://localhost:" <> show port <> "/") $
  setTimeout 9
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

runCmd :: CmdArgs -> IO ()
runCmd = \case
  ws@WebService {} -> do
    $(trIo "start/ws")
    fr <- mkFlakeRepo
    efsTid <- forkFinally (sendEmtpyFlakeSubmition fr 30_000_000)
      (\case
          Left e -> putStrLn $ "Empty Submition Thead ended: " <> show e
          Right () -> putStrLn "Empty Submition Thead ended without errors")
    putStrLn $ "Empty Submition thread is forked " <> show efsTid
    let y = Ypp fr
    logger <- makeLogger y
    case liftA2 mkTlsSettings ws.certFile ws.keyFile of
      Nothing -> runPlain (mkSettings y ws logger) =<< toWaiApp y
      Just tlsSngs -> runTLS tlsSngs (mkSettings y ws logger) =<< toWaiApp y
  FetcherJob serUrl ->
    runFetcher serUrl
  PieceOfFlakeVersion ->
    putStrLn $ "Version " <> showVersion version
