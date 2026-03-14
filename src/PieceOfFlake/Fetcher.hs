module PieceOfFlake.Fetcher where
import Crypto.Hash.SHA1 (hashlazy)
import Data.Aeson qualified as A
import Data.Aeson
    ( FromJSON(parseJSON),
      eitherDecodeStrict,
      genericParseJSON,
      defaultOptions,
      Options(fieldLabelModifier) )
import Data.Char ( toLower )
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import PieceOfFlake.Aeson ( Some, someToList )
import PieceOfFlake.CmdArgs
    ( FetcherCmdArgs(fetcherId, rawNixCacheErrMaxAge,
                     rawNixCacheMaxAge, rawNixCache, looseFlakes),
      RawNixCacheErrorMaxAge,
      RawNixCacheMaxAge,
      FetcherSecret )
import PieceOfFlake.Flake
    ( FlakeUrl(..),
      Architecture(..),
      PackageName(..),
      PackageInfo(..),
      MetaFlake(..) )
import PieceOfFlake.Flake.Repo ( FetcherReq(FetcherReq) )
import PieceOfFlake.Prelude
import PieceOfFlake.Req
    ( DynamicUrl,
      POST(POST),
      ReqBodyJson(ReqBodyJson),
      HttpException(VanillaHttpException),
      HttpConfig(httpConfigRetryJudge, httpConfigRetryJudgeException),
      defaultHttpConfig,
      jsonResponse,
      responseBody,
      runReq,
      dynReq,
      isStatusCodeException,
      responseStatusCode )
import System.Exit ( ExitCode(ExitFailure, ExitSuccess) )
import System.Process ( readProcess, readProcessWithExitCode )
import UnliftIO.Retry
    ( capDelay,
      fibonacciBackoff,
      limitRetries,
      recoverAll,
      recovering )

import UnliftIO.Directory
    ( doesFileExist,
      createDirectoryIfMissing,
      removeFile )
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

isFileOlderThan :: (MonadIO m, ClockMonad m) => Tagged a FilePath -> Tagged a NominalDiffTime -> m Bool
isFileOlderThan (Tagged fp) (Tagged age) = do
  getModificationTime fp >>= (`doAfter` \mt -> do now <- getTimeAfter mt; pure $ now `diffUTCTime` mt > age)

readJsonCached :: (FromJSON a, FetcherM m) => FilePath -> Text -> [Text] -> m a
readJsonCached cacheDir prg prgArgs = do
  let cmd = LBS.intercalate " " (encodeUtf8 <$> (prg : prgArgs))
      (cmdH2, cmdHashRest) = splitAt 2 $ showDigest $ hashlazy cmd
      callDir = cacheDir </> cmdH2 </> cmdHashRest
      exOut = Tagged @RawNixCacheErrorMaxAge $ callDir </> "err.txt"
      outJson = Tagged @RawNixCacheMaxAge $ callDir </> "out.json"
      cmdFile = callDir </> "cmd.sh"
      doDirect = do
        $(logDebug) $ "Direct read " <> show cmd
        createDirectoryIfMissing True callDir
        prgOut <- liftIO $ do
          readProcessWithExitCode (toString prg) (toString <$> prgArgs) "" >>= \case
            (ExitSuccess, out, _err) -> pure out
            (ExitFailure ec, _, err) -> do
              writeFileBS (untag exOut) $ encodeUtf8 err
              throwIO . stringException $ "Command [" <> show cmd <> "] exited with: " <>
                show ec <> " and output: " <> err
        let prgOutBs = encodeUtf8 prgOut
        writeFileBS (untag outJson) prgOutBs
        writeFileLBS cmdFile cmd
        case eitherDecodeStrict prgOutBs  of
          Right x -> do
            pure x
          Left e ->
            throwIO . stringException $
            "Failed to parse output as JSON:\n" <> prgOut <> "\nFrom: " <>
            toString (T.intercalate " " prgArgs)  <> "\nError: " <> e

  $(logInfo) $ "readJsonCached " <> show cmd
  ifM (doesFileExist $ untag exOut)
    (do
      $(logDebug) $ "Ex cache exist for " <> show cmd
      ifM (isFileOlderThan exOut . rawNixCacheErrMaxAge =<< asks fetcherArgs)
        (do $(logDebug) $ "Ex cache is expired for " <> show cmd
            removeFile $ untag exOut
            doDirect)
        (throwIO . stringException . show =<< readFileBS (untag exOut))
    )
    (ifM (doesFileExist $ untag outJson)
      (do
          $(logDebug) $ "Out cache exist for " <> show cmd
          ifM (isFileOlderThan outJson . rawNixCacheMaxAge =<< asks fetcherArgs)
            doDirect
            (do
              $(logDebug) $ "Read cache from: " <> show outJson
              jsonLbs <- readFileBS $ untag outJson
              case eitherDecodeStrict jsonLbs of
                Right x -> pure x
                Left e ->
                  throwIO . stringException $
                  "Failed to parse output as JSON:\n" <> show jsonLbs <> "\nFrom: " <>
                  toString (T.intercalate " " prgArgs)  <> "\nError: " <> e
            )
      )
      doDirect
    )

data FetcherConf
  = FetcherConf
  { fetUrl :: DynamicUrl
  , arch :: Architecture
  , fetcherSecret :: FetcherSecret
  , fetcherArgs :: FetcherCmdArgs
  , flakeLoser :: Maybe (FlakeUrl, Either Text MetaFlake) -> Maybe (FlakeUrl, Either Text MetaFlake)
  }

type FetcherM m = (MonadReader FetcherConf m, PoF m)

readJson :: (FetcherM m, FromJSON a) => Text -> [Text] -> m a
readJson prg prgArgs = do
  asks fetcherArgs <&> rawNixCache >>= \case
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
  , spdxId :: Maybe Text
  , url :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON RawLicense where

newtype RawPlatform = RawPlatform { unRawPlatform :: Text } deriving (Show, Eq)

instance FromJSON RawPlatform where
  parseJSON (A.String x) = pure $ RawPlatform x
  parseJSON _ = pure $ RawPlatform "trash"

data RawPackage
  = RawPackage
  { broken :: Maybe Bool
  , description :: Maybe Text
  , insecure :: Maybe Bool
  , license :: Maybe (Some RawLicense)
  , name :: Maybe Text
  , platforms :: Maybe [RawPlatform]
  , unfree :: Maybe Bool
  , unsupported :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance FromJSON RawPackage

nixEvalPkgMeta :: FetcherM m => FlakeUrl -> Architecture -> PackageName -> m RawPackage
nixEvalPkgMeta (FlakeUrl fu) (Architecture arch) (PackageName pn) =
  readJson "nix" ["eval", "--json", fu <> "#packages." <> arch <> "." <> pn <> ".meta" ]

rawPackageToPackageInfo :: PackageName -> RawPackage -> PackageInfo
rawPackageToPackageInfo pn rp =
  PackageInfo
  { description = rp.description
  , license = shortName <$> join (maybeToList (fmap someToList rp.license))
  , name = maybe pn PackageName rp.name
  , platforms = unRawPlatform <$> fromMaybe [] rp.platforms
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
    getPackageInfo arch pkgName = nixEvalPkgMeta fu arch pkgName <&> rawPackageToPackageInfo pkgName
    mapArchPkgs (arch, pkgNames) =
          (arch, ) . M.fromList <$> mapM (\pkgName -> (pkgName,) <$> getPackageInfo arch pkgName)
            (filter (/= PackageName "default") pkgNames)

uploadFlakeAndFetch :: (FetcherM m) =>
  Maybe (FlakeUrl, Either Text MetaFlake)
  -> m ()
uploadFlakeAndFetch f = do
  fctx <- ask
  let fetr = FetcherReq fctx.fetcherArgs.fetcherId f fctx.fetcherSecret
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
    Just fu -> catchAny (go fctx fu) (onEx fctx fu)
  where
    onEx fctx fu e = do
      $(logError) $ "nix failed for " <> show fu <> " with " <> show e
      uploadFlakeAndFetch $ fctx.flakeLoser (Just (fu, Left $ show e))
    go fctx fu =
      uploadFlakeAndFetch . fctx.flakeLoser . Just . (fu,) . Right =<< metaFlakeFromUrl fu

runFetcher :: PoF m => DynamicUrl -> FetcherCmdArgs -> FetcherSecret -> m ()
runFetcher serviceUrl fa fsec = do
  $(logInfo) $ "Fetcher " <> show fa.fetcherId <> " started for " <> show serviceUrl
  ca <- nixCurrentArch
  forever $ do
    recoverAll
      (capDelay (toMs (6 :: Second)) (fibonacciBackoff (toMs (1 :: Second))))
      (\_ ->
         runReaderT (uploadFlakeAndFetch Nothing)
           $ FetcherConf serviceUrl ca fsec fa looseF)
  where
    looseF :: forall a. Maybe a -> Maybe a
    looseF = if untag fa.looseFlakes
      then const Nothing
      else id
