{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.CmdRun where

import Control.Monad.Logger ( liftLoc, ToLogStr(toLogStr) )
import Data.Version (showVersion)
import Language.Haskell.TH.Syntax (qLocation)
import PieceOfFlake.Page ( Ypp(Ypp) )
import PieceOfFlake.CmdArgs ( CmdArgs(..), CertKey, Cert )
import PieceOfFlake.Flake (mkFlakeRepo)
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
  rs@RunService {} -> do
    $(trIo "start/rs")
    y <- Ypp <$> mkFlakeRepo
    logger <- makeLogger y
    case liftA2 mkTlsSettings rs.certFile rs.keyFile of
      Nothing -> runPlain (mkSettings y rs logger) =<< toWaiApp y
      Just tlsSngs -> runTLS tlsSngs (mkSettings y rs logger) =<< toWaiApp y
  PieceOfFlakeVersion ->
    putStrLn $ "Version " <> showVersion version
