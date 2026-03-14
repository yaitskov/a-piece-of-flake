{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module PieceOfFlake.CmdArgs where

import Data.Either.Combinators ( mapLeft )
import GHC.TypeLits (KnownSymbol)
import Network.HostName ( getHostName )
import Options.Applicative
import PieceOfFlake.Flake ( FetcherId(..) )
import PieceOfFlake.Prelude
import PieceOfFlake.Prelude qualified as P
import PieceOfFlake.Req ( DynamicUrl(UrlHttp), http, port, parseUrl )
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
type IndexQueryCacheSize = "index-query-cache-size"
type BadFlakeMaxAge = "bad-flake-max-age"

newtype RingBufferSize = RingBufferSize (Refined (FromTo 1 12) Int)

instance Show RingBufferSize where
  show = P.show . unrefine . coerce

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
    , logLevel :: LogLevel
    , indexQueryCacheSize :: Tagged IndexQueryCacheSize Word
    , ringBufferSize :: RingBufferSize
    , badFlakeMaxAge :: Tagged BadFlakeMaxAge Second
    , allowResubmitIndexedFlakeIn :: Tagged ResubmitPeriod NominalDiffTime
    }
  deriving Show

type RawNixCacheMaxAge = "nix-cache-max-age"
type RawNixCacheErrorMaxAge = "nix-cache-err-max-age"
type LooseFlakes = "loose-flake"
data FetcherCmdArgs
  = FetcherCmdArgs
    { webServiceUrl :: DynamicUrl
    , rawNixCache :: Tagged RawNixCacheOutput (Maybe FilePath)
    , fetcherId :: FetcherId
    , fetcherSecretPath :: Tagged FetcherSecret FilePath
    , noSubmitionHeartbeat :: Tagged NoSubmitionHeartbeatSec Second
    , rawNixCacheMaxAge :: Tagged RawNixCacheMaxAge NominalDiffTime
    , rawNixCacheErrMaxAge :: Tagged RawNixCacheErrorMaxAge NominalDiffTime
    , logLevel :: LogLevel
    , looseFlakes :: Tagged LooseFlakes Bool
    }
  deriving (Show)

data SubmitListOfFlakesArgs
  = SubmitListOfFlakesArgs
  { webServiceUrl :: DynamicUrl
  , logLevel :: LogLevel
  , indexTimeoutIn :: Tagged ResubmitPeriod NominalDiffTime
  }
  deriving (Show)

data CmdArgs
  = WebService WsCmdArgs
  | FetcherJob FetcherCmdArgs
  | SubmitListOfFlakes SubmitListOfFlakesArgs
  | PieceOfFlakeVersion
  deriving (Show)

execWithArgs :: MonadIO m => (CmdArgs -> m a) -> [String] -> m a
execWithArgs a args = a =<< liftIO (handleParseResult $ execParserPure defaultPrefs (info (cmdp <**> helper) phelp) args)
  where
    serviceP = WebService <$> (WsCmdArgs <$> portOption <*> certO <*>
      certKeyO <*> acidOption <*> cacheSecondsO <*>
      baseUrlO <*> fetcherSecretPathO <*> noSubmitionHeartbeatO <*>
      allowResubmitBadFlakeInO <*> logLevelO <*>
      indexQueryCacheSizeO <*> ringBufferSizeO <*> badFlakeMaxAgeO <*>
      allowResubmitIndexedFlakeInO)
    fetcherP = FetcherJob <$> (FetcherCmdArgs <$> urlOption <*> rawNixCacheO <*>
      customFetcherIdO <*> fetcherSecretPathO <*> noSubmitionHeartbeatO <*>
      rawNixCacheMaxAgeO @RawNixCacheMaxAge (7 * 24 * 3600) <*>
      rawNixCacheMaxAgeO @RawNixCacheErrorMaxAge 800 <*>
      logLevelO <*> looseFlakesO)
    submitListP = SubmitListOfFlakes <$>
      (SubmitListOfFlakesArgs <$> urlOption <*> logLevelO <*> allowResubmitBadFlakeInO)
    cmdp =
      hsubparser
        (  command "web" (infoP serviceP "launch web service")
        <> command "fetcher" (infoP fetcherP "launch fetcher job")
        <> command "submit-list" (infoP submitListP "submit list of flakes read for stdin - an url per line")
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

allowResubmitIndexedFlakeInO :: Parser (Tagged ResubmitPeriod NominalDiffTime)
allowResubmitIndexedFlakeInO = Tagged <$>
  option auto
  ( long "resubmit-indexed-flake-interval"
    <> showDefault
    <> value (toNominal (2 :: Hour))
    <> help "how soon an indexed flake can be resubmitted"
  )

looseFlakesO :: Parser (Tagged LooseFlakes Bool)
looseFlakesO = Tagged <$>
  option auto
  ( long (symbolVal $ Proxy @LooseFlakes)
    <> showDefault
    <> value False
    <> help "loose fetched flake - debugging web service response"
  )

logLevelO :: Parser LogLevel
logLevelO =
  option auto
  ( long "log-level"
    <> short 'l'
    <> showDefault
    <> value LevelDebug
    <> help "app log level"
    <> metavar "LOG"
  )

rawNixCacheMaxAgeO :: forall a. KnownSymbol a => NominalDiffTime -> Parser (Tagged a NominalDiffTime)
rawNixCacheMaxAgeO defVal =
 Tagged <$>
  option auto
  ( long (symbolVal $ Proxy @a)
    <> showDefault
    <> value defVal
    <> help "max age of cached entity"
  )

badFlakeMaxAgeO :: Parser (Tagged BadFlakeMaxAge Second)
badFlakeMaxAgeO = Tagged <$>
  option auto
  ( long (symbolVal $ Proxy @BadFlakeMaxAge)
    <> showDefault
    <> value (8 * 3600)
    <> help "bad flake max age"
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

ringBufferSizeO :: Parser RingBufferSize
ringBufferSizeO =
  option (eitherReader (mapLeft toString . readEither >=> fmap RingBufferSize . mapLeft P.show . refine))
  ( long "ring-buffer"
    <> showDefault
    <> value (RingBufferSize $$(refineTH 3))
    <> help "ring buffer size that  is used for mean values"
  )

indexQueryCacheSizeO :: Parser (Tagged IndexQueryCacheSize Word)
indexQueryCacheSizeO = Tagged <$>
  option auto
  ( long (symbolVal $ Proxy @IndexQueryCacheSize)
    <> showDefault
    <> value 100
    <> help "index query cache size"
  )

cacheSecondsO :: Parser (Tagged StaticCacheSeconds Word32)
cacheSecondsO = Tagged <$>
  option auto
  ( long "static-cache"
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
