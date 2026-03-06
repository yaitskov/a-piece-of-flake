{-# LANGUAGE DuplicateRecordFields #-}
module PieceOfFlake.CmdArgs where

import Data.Aeson ( FromJSON, ToJSON )
import Data.Either.Combinators ( mapLeft )
import Network.HostName ( getHostName )
import Options.Applicative
import PieceOfFlake.Flake ( FetcherId(..) )
import PieceOfFlake.Prelude
import PieceOfFlake.Prelude qualified as P
import PieceOfFlake.Req
    ( DynamicUrl(UrlHttp), http, port, parseUrl )
import Text.Show ( Show(show) )


data HttpPort
data Cert
data CertKey
data AcidFlakesPath
data StaticCacheSeconds
data RawNixCacheOutput
data BaseUrl
data NoSubmitionHeartbeatSec
data ResubmitPeriod
newtype FetcherSecret = FetcherSecret Text deriving (Eq, Generic, FromJSON, ToJSON)
instance Show FetcherSecret where
  show _ = "****"

data WsCmdArgs
  = WsCmdArgs
    { httpPortToListen :: Tagged HttpPort Int
    , certFile :: Maybe (Tagged Cert FilePath)
    , keyFile :: Maybe (Tagged CertKey FilePath)
    , acidFlakes :: Tagged AcidFlakesPath FilePath
    , staticCache :: Tagged StaticCacheSeconds Word32
    , baseUrl :: Tagged BaseUrl Text
    , fetcherSecretPath :: Tagged FetcherSecret FilePath
    , noSubmitionHeartbeat :: Tagged NoSubmitionHeartbeatSec Second
    , allowResubmitBadFlakeIn :: Tagged ResubmitPeriod NominalDiffTime
    }
  deriving Show

data FetcherCmdArgs
  = FetcherCmdArgs
    { webServiceUrl :: DynamicUrl
    , rawNixCache :: Tagged RawNixCacheOutput (Maybe FilePath)
    , fetcherId :: FetcherId
    , fetcherSecretPath :: Tagged FetcherSecret FilePath
    , noSubmitionHeartbeat :: Tagged NoSubmitionHeartbeatSec Second
    }
  deriving Show

data CmdArgs
  = WebService WsCmdArgs
  | FetcherJob FetcherCmdArgs
  | PieceOfFlakeVersion
  deriving Show

execWithArgs :: MonadIO m => (CmdArgs -> m a) -> [String] -> m a
execWithArgs a args = a =<< liftIO (handleParseResult $ execParserPure defaultPrefs (info (cmdp <**> helper) phelp) args)
  where
    serviceP = WebService <$> (WsCmdArgs <$> portOption <*> certO <*>
      certKeyO <*> acidOption <*> cacheSecondsO <*>
      baseUrlO <*> fetcherSecretPathO <*> noSubmitionHeartbeatO <*>
      allowResubmitBadFlakeInO)
    fetcherP = FetcherJob <$> (FetcherCmdArgs <$> urlOption <*> rawNixCacheO <*>
      customFetcherIdO <*> fetcherSecretPathO <*> noSubmitionHeartbeatO)
    cmdp =
      hsubparser
        (  command "web" (infoP serviceP "launch web service")
        <> command "fetcher" (infoP fetcherP "launch fetcher job")
        <> command "version" (infoP (pure PieceOfFlakeVersion) "print program version"))

    infoP p h = info p (progDesc h <> fullDesc)
    phelp =
      progDesc
        "Nix Flake repository"

defaultPort :: Int
defaultPort = 3003

allowResubmitBadFlakeInO :: Parser (Tagged ResubmitPeriod NominalDiffTime)
allowResubmitBadFlakeInO = Tagged <$>
  option auto
  ( long "resubmit-bad-flake-interval"
    <> showDefault
    <> value 600
    <> help "how soon bad flake can be resubmitted"
  )
noSubmitionHeartbeatO :: Parser (Tagged NoSubmitionHeartbeatSec Second)
noSubmitionHeartbeatO = Tagged <$>
  option auto
  ( long "heartbeat"
    <> short 'b'
    <> showDefault
    <> value 600
    <> help "period for putting empty submition request into fetcher queue (in seconds) when queue is empty"
    <> metavar "HEARTBEAT"
  )

cacheSecondsO :: Parser (Tagged StaticCacheSeconds Word32)
cacheSecondsO = Tagged <$>
  option auto
  ( long "static-cache"
    <> short 'c'
    <> showDefault
    <> value 1
    <> help "cache duration for static content (used in HTTP header)"
    <> metavar "STATIC_CACHE"
  )

rawNixCacheO :: Parser (Tagged RawNixCacheOutput (Maybe FilePath))
rawNixCacheO = Tagged <$>
  option (eitherReader parse )
  ( long "raw-nix-cache-output"
    <> short 'c'
    <> showDefault
    <> value (pure "fetcher-raw-nix-cache")
    <> help "path to fetcher cache of nix raw output"
    <> metavar "RAW_NIX_CACHE"
  )
  where
    parse = \case
      "" -> pure Nothing
      "null" -> pure Nothing
      "-" -> pure Nothing
      o -> pure $ Just o

customFetcherIdO :: Parser FetcherId
customFetcherIdO = FetcherId . toText <$>
  option str
  ( long "fetcher-id"
    <> short 'i'
    <> showDefault
    <> value (unsafePerformIO getHostName)
    <> help "fetcher id"
    <> metavar "FID"
  )

fetcherSecretPathO :: Parser (Tagged FetcherSecret FilePath)
fetcherSecretPathO = Tagged <$>
  option str
  ( long "fetcher-secret"
    <> short 's'
    <> showDefault
    <> value ".fetcher-secret"
    <> help "path to file with secret for fetcher authentication on web service"
    <> metavar "SECRET"
  )

acidOption :: Parser (Tagged AcidFlakesPath FilePath)
acidOption = Tagged <$>
  option str
  ( long "acid"
    <> short 'a'
    <> showDefault
    <> value  "acid-flakes/"
    <> help "path do ACID flake store"
    <> metavar "ACID"
  )

baseUrlO :: Parser (Tagged BaseUrl Text)
baseUrlO = Tagged <$>
  option str
  ( long "base-url"
    <> short 'u'
    <> showDefault
    <> value ("http://localhost:" <> P.show defaultPort)
    <> help "base web service url for HTML links"
    <> metavar "URL"
  )

urlOption :: Parser DynamicUrl
urlOption =
  option (eitherReader (mapLeft toString . parseUrl . toText))
  ( long "url"
    <> short 'u'
    <> showDefault
    <> value (UrlHttp (http "localhost") (port defaultPort))
    <> help "web service url for fetching flake submition requests and uploading flake meta back"
    <> metavar "URL"
  )

portOption :: Parser (Tagged HttpPort Int)
portOption = Tagged <$>
  option auto
  ( long "port"
    <> short 'p'
    <> showDefault
    <> value defaultPort
    <> help "HTTP(S) port to listen"
    <> metavar "PORT"
  )

emptyToNothing :: FilePath -> Maybe FilePath
emptyToNothing "" = Nothing
emptyToNothing s = Just s

zeroToNothing :: Int -> Maybe Int
zeroToNothing s | s <= 0 = Nothing
                | otherwise = Just s

pured :: (Applicative g, Applicative f) => g a -> g (f a)
pured = fmap pure

certO :: Parser (Maybe (Tagged Cert FilePath))
certO = pured . emptyToNothing <$>
  strOption
  ( long "certificate"
    <> short 'c'
    <> value ""
    <> help "path to SSL certificate file (./certificate.pem)"
    <> metavar "CERT"
  )
certKeyO :: Parser (Maybe (Tagged CertKey FilePath))
certKeyO = pured . emptyToNothing <$>
  strOption
  ( long "key"
    <> short 'k'
    <> value ""
    <> help "path to key file of SSL certificate (./key.pem)"
    <> metavar "KEY"
  )
