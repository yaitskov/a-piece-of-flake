module PieceOfFlake.CmdArgs where

import Data.Either.Combinators ( mapLeft )
import Options.Applicative
import PieceOfFlake.Prelude
import PieceOfFlake.Req
    ( DynamicUrl(UrlHttp), http, port, parseUrl )

data HttpPort
data Cert
data CertKey
data AcidFlakesPath
data StaticCacheSeconds
data RawNixCacheOutput

data CmdArgs
  = WebService
    { httpPortToListen :: Tagged HttpPort Int
    , certFile :: Maybe (Tagged Cert FilePath)
    , keyFile :: Maybe (Tagged CertKey FilePath)
    , acidFlakes :: Tagged AcidFlakesPath FilePath
    , staticCache :: Tagged StaticCacheSeconds Word32
    }
  | FetcherJob
    { webServiceUrl :: DynamicUrl
    , rawNixCache :: Tagged RawNixCacheOutput (Maybe FilePath)
    }
  | PieceOfFlakeVersion
  deriving Show

execWithArgs :: MonadIO m => (CmdArgs -> m a) -> m a
execWithArgs a = a =<< liftIO (execParser $ info (cmdp <**> helper) phelp)
  where
    serviceP = WebService <$> portOption <*> certO <*> certKeyO <*> acidOption <*> cacheSecondsO
    fetcherP = FetcherJob <$> urlOption <*> rawNixCacheO
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
