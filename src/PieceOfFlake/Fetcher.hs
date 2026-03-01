{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.Fetcher where


import Control.Monad.Catch ( Handler(Handler) )
import Data.Aeson
    ( FromJSON(parseJSON),
      defaultOptions,
      Options(fieldLabelModifier),
      eitherDecodeStrict,
      genericParseJSON )
import Data.Char ( toLower )
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import PieceOfFlake.Req
    ( MonadHttp,
      DynamicUrl,
      POST(POST),
      ReqBodyJson(ReqBodyJson),
      defaultHttpConfig,
      jsonResponse,
      responseBody,
      runReq,
      dynReq,
      HttpException(VanillaHttpException) )
import PieceOfFlake.Flake
    ( PackageInfo(..),
      PackageName(..),
      FlakeUrl(..),
      MetaFlake(..),
      Architecture(..) )
import PieceOfFlake.Prelude
import System.Process ( readProcess )
import UnliftIO ( MonadUnliftIO, catchAny, stringException, throwIO )
import UnliftIO.Retry



readJson :: (FromJSON a, MonadIO m) => Text -> [Text] -> m a
readJson prg prgArgs = do
  prgOut <- liftIO (readProcess (toString prg) (toString <$> prgArgs) "")
  case eitherDecodeStrict $ encodeUtf8 prgOut of
    Right x -> pure x
    Left e ->
      throwIO . stringException $
      "Failed to parse output as JSON:\n" <> prgOut <> "\nFrom: " <>
      toString (T.intercalate " " prgArgs)  <> "\nError: " <> e

data RawFlakeOrigin
  = RawFlakeOrigin
  { flakeOriginType :: Text
  , flakeOriginOwner :: Maybe Text
  , flakeOriginRepo :: Maybe Text
  , flakeOriginRev :: Maybe Text
  , flakeOriginUrl :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON RawFlakeOrigin where
  parseJSON = genericParseJSON
    (defaultOptions { fieldLabelModifier = fmap  toLower . drop (length ("flakeOrigin" :: String)) })

data RawNode
  = RawNode
  { flake :: Maybe Bool
  , original :: Maybe RawFlakeOrigin
  } deriving (Show, Eq, Generic)
instance FromJSON RawNode

newtype RawLocks
  = RawLocks
  { nodes :: Map Text RawNode
  } deriving (Show, Eq, Generic)
instance FromJSON RawLocks

data RawFlakeInfo
  = RawFlakeInfo
  { revision :: Text
  , description :: Maybe Text
  , lastModified :: Integer
  , locks :: RawLocks
  } deriving (Show, Eq, Generic)
instance FromJSON RawFlakeInfo

nixFlakeInfo :: MonadIO m => FlakeUrl -> m RawFlakeInfo
nixFlakeInfo (FlakeUrl fu) =
  readJson "nix" ["flake", "metadata", "--json", fu]


nixCurrentArch :: MonadIO m => m Architecture
nixCurrentArch =
  Architecture . toText <$>
  liftIO (readProcess "nix" (toString <$> words "eval --impure --raw --expr builtins.currentSystem") "")

newtype RawFlakeOutputs
  = RawFlakeOutputs
  { packages :: Map Architecture (Map PackageName ())
  } deriving (Show, Eq, Generic)

instance FromJSON RawFlakeOutputs


nixFlakeShow :: MonadIO m => FlakeUrl -> m RawFlakeOutputs
nixFlakeShow (FlakeUrl fu) =
  readJson "nix" ["flake", "show", "--json", fu]

data RawLicense
  = RawLicense
  { deprecated :: Bool
  , free :: Bool
  , fullName :: Text
  , shortName :: Text
  , spdxId :: Text
  , url :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON RawLicense

data RawPackage
  = RawPackage
  { broken :: Bool
  , description :: Text
  , insecure :: Bool
  , license :: RawLicense
  , name :: Text
  , platforms :: [Text]
  , unfree :: Bool
  , unsupported :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON RawPackage

nixEvalPkgMeta :: MonadIO m => FlakeUrl -> Architecture -> PackageName -> m RawPackage
nixEvalPkgMeta (FlakeUrl fu) (Architecture arch) (PackageName pn) =
  readJson "nix" ["eval", "--json", fu <> "#packages." <> arch <> "." <> pn <> ".meta" ]

rawPackageToPackageInfo :: RawPackage -> PackageInfo
rawPackageToPackageInfo rp =
  PackageInfo
  { description = rp.description
  , license = rp.license.shortName
  , name = PackageName rp.name
  , platforms = rp.platforms
  , unfree = rp.unfree
  , broken = rp.broken
  }

metaFlakeFromUrl :: (MonadReader Architecture m, MonadUnliftIO m) => FlakeUrl -> m MetaFlake
metaFlakeFromUrl fu = do
  rfi <- nixFlakeInfo fu
  curArch <- ask
  archPkgs <- fmap M.keys . M.filterWithKey (\a _ps -> a == curArch) . (\x -> x.packages) <$> nixFlakeShow fu
  metaPackages <- M.fromList <$> mapM mapArchPkgs (M.toList archPkgs)
  pure MetaFlake
    { description = rfi.description
    , packages = metaPackages
    , rev = rfi.revision
    , flakeDeps = []
    }
  where
    getPackageInfo arch pkgName =
       rawPackageToPackageInfo <$> nixEvalPkgMeta fu arch pkgName
    mapArchPkgs (arch, pkgNames) =
          (arch, ) . M.fromList <$> mapM (\pkgName -> (pkgName,) <$> getPackageInfo arch pkgName)
            (filter (/= PackageName "default") pkgNames)

uploadFlakeAndFetch :: (MonadReader Architecture m, MonadHttp m, MonadUnliftIO m) =>
  DynamicUrl ->
  Maybe (FlakeUrl, Either Text MetaFlake)
  -> m ()
uploadFlakeAndFetch surl f = do
  jr <- dynReq POST surl "fetch-new-flake-submitions" (ReqBodyJson f) jsonResponse
  case responseBody jr of
    Nothing -> uploadFlakeAndFetch surl Nothing
    Just fu -> catchAny (go fu) (onEx fu)
  where
    onEx fu e =
      uploadFlakeAndFetch surl (Just (fu, Left $ show e))
    go fu =
      uploadFlakeAndFetch surl . Just . (fu,) . Right =<< metaFlakeFromUrl fu

-- xxx = MetaFlake
--   { description = Just "VPN bypass"
--   , packages = fromList
--     [("aarch64-darwin",fromList [])
--     ,("aarch64-linux",fromList [])
--     ,("x86_64-darwin",fromList [])
--     ,("x86_64-linux",fromList [])]
--   , rev = "29aaa5f650c4d08932646399aaad0904322f5536"
--   , flakeDeps = []
--   }

runFetcher :: MonadUnliftIO m => DynamicUrl -> m ()
runFetcher serviceUrl = do
  ca <- nixCurrentArch
  recovering
    (fibonacciBackoff 100_000 <> limitRetries 1111)
    [ \_rs -> Handler $
        \case
          e@VanillaHttpException {} -> do
            putStrLn $ "Retry after " <> show e
            pure True
          _ -> pure False
    ]
    (\_ -> runReq defaultHttpConfig $ runReaderT (uploadFlakeAndFetch serviceUrl Nothing) ca)
