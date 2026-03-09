module PieceOfFlake.Index where

import Data.HashPSQ qualified as PSQ
import Data.Map.Strict qualified as M
import Data.LruCache qualified as LRU
import Data.LruCache.Internal qualified as LRU
import Data.LruCache (LruCache)
import Data.SearchEngine
    ( Term,
      SearchConfig(documentFeatureValue, SearchConfig, documentKey,
                   extractDocumentTerms, transformQueryTerm),
      SearchRankParameters(paramAutosuggestPostfilterLimit,
                           SearchRankParameters, paramK1, paramB, paramFieldWeights,
                           paramFeatureWeights, paramFeatureFunctions,
                           paramResultsetSoftLimit, paramResultsetHardLimit,
                           paramAutosuggestPrefilterLimit),
      SearchEngine,
      FeatureFunction(LogarithmicFunction),
      query,
      initSearchEngine,
      insertDoc,
      queryAutosuggest,
      ResultsFilter(NoFilter) )
import ListT qualified as L
import NLP.Tokenize.Text ( tokenize )
import PieceOfFlake.Flake
    ( FlakeUrl,
      repoOfFlakeUrl,
      PackageInfo(broken, description, license, name, unfree),
      MetaFlake(hasNixOsModules, description, packages),
      Flake(meta, uploadedAt, FlakeIndexed, FlakeFetched, flakeUrl),
      isIndexed )
import PieceOfFlake.CmdArgs ( IndexQueryCacheSize )
import PieceOfFlake.Prelude hiding (pi, Map)
import PieceOfFlake.Stm
    ( readTQueue, writeTQueue, TQueue, newTQueueIO, atomicalog )
import StmContainers.Map ( insert, listTNonAtomic, lookup, Map )
import PieceOfFlake.Stats
    ( RepoStatsF(meanTimeInIndexQueue, fetchedFlakes, indexedFlakes,
                 meanIndexTime),
      addTimeDif )
type FlakeSearchEngine = SearchEngine (FlakeUrl, MetaFlake) FlakeUrl () ()

data FlakeIndex
  = FlakeIndex
  { searchEngine :: TVar FlakeSearchEngine
  , indexerQueue :: TQueue FlakeUrl
  , indexerQueueLen :: TVar Int
  , queryCache :: TVar (LruCache Text UtcBox)
  , searchRequestCounter :: TVar Integer
  }

mkFlakeIndex :: MonadIO m => Tagged IndexQueryCacheSize Word -> m FlakeIndex
mkFlakeIndex (Tagged cs) = do
  liftIO $
    FlakeIndex <$>
      newTVarIO emptySearchEngine <*>
      newTQueueIO <*>
      newTVarIO 0 <*>
      newTVarIO (LRU.empty $ fromIntegral cs) <*>
      newTVarIO 0

packageInfoToTerms :: PackageInfo -> [Term]
packageInfoToTerms pi =
  tokenize pi.description <>
  [ "license", pi.license
  , toText pi.name
  , if pi.unfree then "unfree" else "free"
  , if pi.broken then "broken" else "unbroken"
  ]

extractTerms :: (FlakeUrl, MetaFlake) -> () -> [Term]
extractTerms (fu, mf) () =
  concatMap tokenize (maybeToList mf.description)
  <> [repoOfFlakeUrl fu] <> (toText <$> M.keys mf.packages)
  <> (concatMap packageInfoToTerms .  concatMap M.elems $ M.elems mf.packages)
  <> memptyIfFalse mf.hasNixOsModules ["nixosModules"]

emptySearchEngine :: FlakeSearchEngine
emptySearchEngine =
  initSearchEngine
    SearchConfig
    { documentKey = fst
    , extractDocumentTerms = extractTerms
    , transformQueryTerm = const
    , documentFeatureValue = \_doc () -> 1
    }
    SearchRankParameters
    { paramK1 = 2 -- weight of repetition
    , paramB = const 0.4 -- normalize by length
    , paramFieldWeights = const 1
    , paramFeatureWeights = const 1
    , paramFeatureFunctions = const $ LogarithmicFunction 2
    , paramResultsetSoftLimit = 10
    , paramResultsetHardLimit = 30
    , paramAutosuggestPrefilterLimit = 10
    , paramAutosuggestPostfilterLimit = 10
    }

data FlakeIndexedSuc
  = FlakeIndexedSuc
  { inIndexQueue :: NominalDiffTime
  , indexingTook :: NominalDiffTime
  }

nothing :: Monad m => m a1 -> m (Maybe a2)
nothing m = do
  void m
  pure Nothing

consumeIndexQueue :: PoF m => RepoStatsF TVar -> Map FlakeUrl Flake -> FlakeIndex -> m ()
consumeIndexQueue rs fs fi = do
  r <- atomicalog $ do
    $(logInfo) "Wait for flakes to index for full text search"
    fu <- lift $ readTQueue fi.indexerQueue
    lift $ modifyTVar' fi.indexerQueueLen (\x -> x - 1)
    ql <- lift $ readTVar fi.indexerQueueLen
    $(logInfo) $ "Start index flake " <> show fu <> "; index queue " <> show ql
    lift (lookup fu fs) >>= \case
      Nothing -> do
        $(logError) $ "Flake " <> show fu <> " is missing"
        pure Nothing
      Just f -> indexFlake rs fi fs f
  mapM_ (\FlakeIndexedSuc { inIndexQueue, indexingTook } -> do
             addTimeDif rs.meanIndexTime indexingTook
             addTimeDif rs.meanTimeInIndexQueue inIndexQueue
        ) r


indexNewFlake :: (MonadLogger (t STM), MonadTrans t) => FlakeIndex -> FlakeUrl -> t STM ()
indexNewFlake fi fu = do
  iql <- lift $ do
    writeTQueue fi.indexerQueue fu
    modifyTVar' fi.indexerQueueLen (1 +)
    readTVar fi.indexerQueueLen
  $(logInfo) $ "Indexer queue increased to " <> show iql

indexFlake :: (MonadLogger (t STM), MonadTrans t) =>
  RepoStatsF TVar -> FlakeIndex -> Map FlakeUrl Flake -> Flake -> t STM (Maybe FlakeIndexedSuc)
indexFlake rs  =
  indexFlake' $ do
    lift $ do
      modifyTVar' rs.fetchedFlakes (flip (-) 1)
      modifyTVar' rs.indexedFlakes (1 +)

indexFlake' :: (MonadLogger (t STM), MonadTrans t) =>
  t STM () -> FlakeIndex -> Map FlakeUrl Flake -> Flake -> t STM (Maybe FlakeIndexedSuc)
indexFlake' onIndexed fi fs f =
  case f of
   ff@FlakeFetched { flakeUrl, uploadedAt } ->
     doAfter uploadedAt $ \ua -> do
       beforeIndex <- lift $ getTimeAfter ua
       let fu = flakeUrl
           ixf = FlakeIndexed fu (mkUtcBox beforeIndex) ff.meta
       onIndexed
       lift $ do
         insert ixf fu fs
         modifyTVar' fi.searchEngine (insertDoc (fu, ff.meta))
       afterIndex <- lift $ getTimeAfter beforeIndex
       $(logInfo) $ "Finished index flake " <> show fu
       pure . Just $ FlakeIndexedSuc
         { inIndexQueue = beforeIndex `diffUTCTime` ua
         , indexingTook = afterIndex `diffUTCTime` beforeIndex
         }
   _nff -> do
     $(logError) $ "Flake " <> show f.flakeUrl <> " is not in the fetched state"
     pure Nothing
newtype Nz a = Nz a

notZero :: (Eq a, Num a) => a -> Maybe (Nz a)
notZero 0 = Nothing
notZero x = pure $ Nz x

divNz :: Fractional a => a -> Nz a -> a
divNz a (Nz b) = a / b

realToFracNz :: (Real a, Fractional b) => Nz a -> Nz b
realToFracNz (Nz x) = Nz $ realToFrac x

loadIndexFromScratch ::
  PoF m => RepoStatsF TVar -> FlakeIndex -> Map FlakeUrl Flake -> [(FlakeUrl, Flake)] -> m ()
loadIndexFromScratch rs fi fsm fs = do
  started <- getCurrentTime
  totalIndexed :: Int <- atomicalog $ do
    $(logInfo) "Started init full text search index population"
    r <- foldlM go 0 fs
    $(logInfo) "Ended init full text search index population"
    pure r
  dur <- flip diffUTCTime started <$> getTimeAfter started
  forM_ (realToFracNz <$> notZero totalIndexed) $ \ti ->
    addTimeDif rs.meanIndexTime $ dur `divNz` ti
  where
    go (i :: Int) = \case
      (_, ff@FlakeFetched {}) ->
        (i + ) . maybe 0 (const 1) <$> indexFlake' (lift $ modifyTVar' rs.indexedFlakes (1 +)) fi fsm ff
      (fu, nff) -> do
        lift $ insert nff fu fsm
        pure i

data FlakeSearchReq
  = FlakeSearchReq
  { searchPattern :: [Text]
  , skipBroken :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON FlakeSearchReq

listQueryCache :: PoF m => FlakeIndex -> m [ Text ]
listQueryCache fi =
  reverse . fmap (^._1) . sortWith (^._2) . PSQ.toList . LRU.lruQueue <$> readTVarIO fi.queryCache

findFlakes :: PoF m =>
  Map FlakeUrl Flake -> FlakeIndex -> FlakeSearchReq -> m [ FlakeUrl ]
findFlakes fs fi FlakeSearchReq { searchPattern = ps } =
  case concatMap tokenize ps of
    [] -> justLoadFirstNFlakes fs 30
    pst@(t1:_) -> do
      now <- mkUtcBox <$> getCurrentTime
      atomicalog (fromIdx now t1 pst)
  where
    fromIdx now t1 pst = do
      se <- lift $ do
        modifyTVar' fi.searchRequestCounter (1 +)
        modifyTVar' fi.queryCache (LRU.insert (unwords ps) now)
        readTVar fi.searchEngine
      $(logInfo) $ "Search flakes by " <> show pst
      let r  = query se pst `alt` (fmap fst . snd $ queryAutosuggest se NoFilter [] t1)
      $(logInfo) $ "Found " <> show (length r) <> " by " <> show ps
      pure r

justLoadFirstNFlakes :: MonadIO m => Map FlakeUrl Flake -> Int -> m [ FlakeUrl ]
justLoadFirstNFlakes fs n =
  fmap fst . filter (isIndexed . snd) <$> liftIO (L.toList $ L.take (2 * n) (listTNonAtomic fs))
