{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
module PieceOfFlake.Fetcher where
import Crypto.Hash.SHA1 (hashlazy)
import Data.Aeson
    ( FromJSON(parseJSON),
      defaultOptions,
      Options(fieldLabelModifier),
      eitherDecodeStrict,
      genericParseJSON )
import Data.Char ( toLower )
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import PieceOfFlake.CmdArgs ( RawNixCacheOutput, FetcherSecret )
import PieceOfFlake.Flake
import PieceOfFlake.Flake.Repo ( FetcherReq(FetcherReq) )
import PieceOfFlake.Prelude
import PieceOfFlake.Req
import System.Process ( readProcess )
import UnliftIO.Retry
    ( capDelay,
      fibonacciBackoff,
      limitRetries,
      recoverAll,
      recovering )
import UnliftIO.Directory
    ( doesFileExist, createDirectoryIfMissing )
import System.FilePath ( (</>) )


showDigest :: ByteString -> String
showDigest = BS8.unpack . convertToBase Base16

readJsonDirect :: (FromJSON a, MonadIO m) => Text -> [Text] -> m a
readJsonDirect prg prgArgs = do
  prgOut <- liftIO (readProcess (toString prg) (toString <$> prgArgs) "")
  case eitherDecodeStrict $ encodeUtf8 prgOut of
    Right x -> pure x
    Left e ->
      throwIO . stringException $
      "Failed to parse output as JSON:\n" <> prgOut <> "\nFrom: " <>
      toString (T.intercalate " " prgArgs)  <> "\nError: " <> e

newtype ReadJsonCached = ReadJsonCached FilePath

{-  hash path cut 2 chars join with </> mkdir -p
    by that path create files out.json and cmd.sh
-}

readJsonCached :: (FromJSON a, MonadIO m) => FilePath -> Text -> [Text] -> m a
readJsonCached cacheDir prg prgArgs = do
  let cmd = LBS.intercalate " " (encodeUtf8 <$> (prg : prgArgs))
      (cmdH2, cmdHashRest) = splitAt 2 $ showDigest $ hashlazy cmd
      callDir = cacheDir </> cmdH2 </> cmdHashRest
      outJson = callDir </> "out.json"
      cmdFile = callDir </> "cmd.sh"
  putLBSLn $ "readJsonCached " <> cmd
  outJsonExist <- doesFileExist outJson
  if outJsonExist
    then do
      jsonLbs <- readFileBS outJson
      case eitherDecodeStrict jsonLbs of
        Right x -> pure x
        Left e ->
          throwIO . stringException $
          "Failed to parse output as JSON:\n" <> show jsonLbs <> "\nFrom: " <>
          toString (T.intercalate " " prgArgs)  <> "\nError: " <> e
    else do
      prgOut <- liftIO (readProcess (toString prg) (toString <$> prgArgs) "")
      let prgOutBs = encodeUtf8 prgOut
      case eitherDecodeStrict prgOutBs  of
        Right x -> do
          createDirectoryIfMissing True callDir
          writeFileBS outJson prgOutBs
          writeFileLBS cmdFile cmd
          pure x
        Left e ->
          throwIO . stringException $
          "Failed to parse output as JSON:\n" <> prgOut <> "\nFrom: " <>
          toString (T.intercalate " " prgArgs)  <> "\nError: " <> e

data FetcherConf
  = FetcherConf
  { fetUrl :: DynamicUrl
  , arch :: Architecture
  , nixCache :: Tagged RawNixCacheOutput (Maybe FilePath)
  , fetcherId :: FetcherId
  , fetcherSecret :: FetcherSecret
  }

type FetcherM m =
  ( MonadReader FetcherConf m
  , MonadIO m
  , MonadUnliftIO m
  , MonadLogger m
  )

readJson :: (FetcherM m, FromJSON a) => Text -> [Text] -> m a
readJson prg prgArgs = do
  asks nixCache >>= \case
    Tagged Nothing -> readJsonDirect prg prgArgs
    Tagged (Just c) -> readJsonCached c prg prgArgs

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

nixFlakeInfo :: FetcherM m => FlakeUrl -> m RawFlakeInfo
nixFlakeInfo (FlakeUrl fu) =
  readJson "nix" ["flake", "metadata", "--json", fu]

nixCurrentArch :: MonadIO m => m Architecture
nixCurrentArch =
  Architecture . toText <$>
  liftIO (readProcess "nix" (toString <$> words "eval --impure --raw --expr builtins.currentSystem") "")

data RawFlakeOutputs
  = RawFlakeOutputs
  { packages :: Maybe (Map Architecture (Map PackageName ()))
  , nixosModules :: Maybe (Map Text ())
  } deriving (Show, Eq, Generic)

instance FromJSON RawFlakeOutputs

nixFlakeShow :: FetcherM m => FlakeUrl -> m RawFlakeOutputs
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

nixEvalPkgMeta :: FetcherM m => FlakeUrl -> Architecture -> PackageName -> m RawPackage
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

metaFlakeFromUrl :: FetcherM m => FlakeUrl -> m MetaFlake
metaFlakeFromUrl fu = do
  rfi <- nixFlakeInfo fu
  curArch <- asks arch
  rfo <- nixFlakeShow fu
  let archPkgs = fmap M.keys . M.filterWithKey (\a _ps -> a == curArch) $ fromMaybe mempty rfo.packages
  metaPackages <- M.fromList <$> mapM mapArchPkgs (M.toList archPkgs)
  pure MetaFlake
    { description = rfi.description
    , packages = metaPackages
    , rev = rfi.revision
    , hasNixOsModules = not $ null rfo.nixosModules
    , flakeDeps = []
    }
  where
    getPackageInfo arch pkgName =
       rawPackageToPackageInfo <$> nixEvalPkgMeta fu arch pkgName
    mapArchPkgs (arch, pkgNames) =
          (arch, ) . M.fromList <$> mapM (\pkgName -> (pkgName,) <$> getPackageInfo arch pkgName)
            (filter (/= PackageName "default") pkgNames)


uploadFlakeAndFetch :: (FetcherM m) =>
  Maybe (FlakeUrl, Either Text MetaFlake)
  -> m ()
uploadFlakeAndFetch f = do
  fctx <- ask
  let fetr = FetcherReq fctx.fetcherId f fctx.fetcherSecret
      rqb = ReqBodyJson fetr
  $(logDebug) $ "sending " <> show fetr
  jr <- recovering
          (fibonacciBackoff 100_000 <> limitRetries 6)
          [ \_rs -> Handler $
              \case
                he@VanillaHttpException {} -> do
                  $(logError) $ "Vanila Exception " <> show he
                  case isStatusCodeException he of
                    Nothing -> pure False
                    Just r ->
                      case responseStatusCode r of
                        scode ->
                          pure $ scode < 400 || scode >= 500
                oe -> do
                  $(logError) $ "Other Exception " <> show oe
                  pure False
          ]
          (\_ ->
              runReq (defaultHttpConfig
                { httpConfigRetryJudgeException = \_ _ -> False
                , httpConfigRetryJudge = \_ _ -> False
                })
              $ dynReq POST fctx.fetUrl "fetch-new-flake-submitions" rqb jsonResponse)
  $(logDebug) $ "Response from WS for " <> show (fmap fst f) <>  ": "   <> show (responseBody jr)
  case responseBody jr of
    Nothing -> uploadFlakeAndFetch Nothing
    Just fu -> catchAny (go fu) (onEx fu)
  where
    onEx fu e = do
      $(logError) $ "nix failed for " <> show fu <> " with " <> show e
      uploadFlakeAndFetch (Just (fu, Left $ show e))
    go fu =
      uploadFlakeAndFetch . Just . (fu,) . Right =<< metaFlakeFromUrl fu

runFetcher :: (MonadLogger m, MonadUnliftIO m) =>
  DynamicUrl ->
  Tagged RawNixCacheOutput (Maybe FilePath) ->
  FetcherId ->
  FetcherSecret ->
  m ()
runFetcher serviceUrl rawNixCa fid fsec = do
  $(logInfo) $ "Fetcher " <> show fid <> " started for " <> show serviceUrl
  ca <- nixCurrentArch
  forever $ do
    recoverAll
      (capDelay (toMs (6 :: Second)) (fibonacciBackoff (toMs (1 :: Second))))
      (\_ ->
         runReaderT (uploadFlakeAndFetch Nothing)
           $ FetcherConf serviceUrl ca rawNixCa fid fsec)
