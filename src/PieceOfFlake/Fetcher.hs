{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.Fetcher where

import Data.Aeson
import Data.Char ( toLower )
import Data.Map.Strict qualified as M

import PieceOfFlake.Req
    ( defaultHttpConfig,
      jsonResponse,
      responseBody,
      runReq,
      MonadHttp,
      POST(POST),
      ReqBodyJson(ReqBodyJson),
      DynamicUrl,
      dynReq )
import PieceOfFlake.Flake

import PieceOfFlake.Prelude
import System.Process
import UnliftIO
-- import UnliftIO.Exception
import Data.Text qualified as T

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

metaFlakeFromUrl :: (MonadUnliftIO m) => FlakeUrl -> m MetaFlake
metaFlakeFromUrl fu = do
  rfi <- nixFlakeInfo fu
  archPkgs <- fmap M.keys . (\x -> x.packages) <$> nixFlakeShow fu
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
          (arch, ) . M.fromList <$> mapM (\pkgName -> (pkgName,) <$> getPackageInfo arch pkgName) pkgNames

uploadFlakeAndFetch :: (MonadHttp m, MonadUnliftIO m) =>
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

runFetcher :: MonadIO m => DynamicUrl -> m ()
runFetcher serviceUrl =
  runReq defaultHttpConfig $ do
    uploadFlakeAndFetch serviceUrl Nothing
