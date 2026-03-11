module PieceOfFlake.Http where

import Data.ByteString qualified as BS
import Language.Haskell.TH.Syntax (qLocation)
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
import Network.Wai
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Middleware.MethodOverride ( methodOverride )
import Network.Wai.Middleware.Gzip
    ( gzip, defaultGzipSettings, GzipSettings(gzipCheckMime) )
import Network.Wai.Middleware.RequestLogger
    ( OutputFormat(Apache),
      defaultRequestLoggerSettings,
      mkRequestLogger,
      Destination(Logger),
      RequestLoggerSettings(outputFormat, destination),
      IPAddrSource(FromSocket) )
import PieceOfFlake.CmdArgs
import PieceOfFlake.Page ( Ypp )
import PieceOfFlake.Prelude
import Yesod.Core
    ( Yesod(makeLogger, messageLoggerSource), YesodDispatch
    , toWaiAppYre, makeSessionBackend, getGetMaxExpires
    , defaultGen
    )
import Yesod.Core.Types ( Logger, YesodRunnerEnv (..), loggerSet )

toWaiApp :: YesodDispatch site => site -> IO Application
toWaiApp site = do
    logger <- makeLogger site
    toWaiAppLogger logger site

toWaiAppLogger :: YesodDispatch site => Logger -> site -> IO Application
toWaiAppLogger logger site = do
    sb <- makeSessionBackend site
    getMaxExpires <- getGetMaxExpires
    let yre = YesodRunnerEnv
                { yreLogger = logger
                , yreSite = site
                , yreSessionBackend = sb
                , yreGen = defaultGen
                , yreGetMaxExpires = getMaxExpires
                }
    messageLoggerSource
        site
        logger
        $(qLocation >>= liftLoc)
        "yesod-core"
        LevelInfo
        (toLogStr ("Application launched" :: ByteString))
    middleware <- mkDefaultMiddlewares logger
    return $ middleware $ toWaiAppYre yre

mkDefaultMiddlewares :: Logger -> IO Middleware
mkDefaultMiddlewares logger = do
    logWare <- mkRequestLogger
      defaultRequestLoggerSettings
        { destination = Network.Wai.Middleware.RequestLogger.Logger $ loggerSet logger
        , outputFormat = Apache FromSocket
        }
    return $ logWare . defaultMiddlewaresNoLogging

defaultMiddlewaresNoLogging :: Middleware
defaultMiddlewaresNoLogging = acceptOverride . autohead . gzip gzipSettings . methodOverride
  where
    gzipSettings = defaultGzipSettings { gzipCheckMime = check }
    check = BS.isPrefixOf "text/html"

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

runWebService :: MonadIO m => WsCmdArgs -> Ypp -> m ()
runWebService ws y = liftIO $ do
  logger <- makeLogger y
  case liftA2 mkTlsSettings ws.certFile ws.keyFile of
    Nothing -> runPlain (mkSettings y ws logger) =<< toWaiApp y
    Just tlsSngs -> runTLS tlsSngs (mkSettings y ws logger) =<< toWaiApp y
