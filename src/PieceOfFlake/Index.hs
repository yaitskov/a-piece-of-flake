{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
      Flake(flakeUrl, FlakeIndexed, FlakeFetched, meta),
      PackageInfo(broken, description, license, name, unfree),
      MetaFlake(hasNixOsModules, description, packages),
      isIndexed,
      repoOfFlakeUrl )
import PieceOfFlake.CmdArgs ( IndexQueryCacheSize )
import PieceOfFlake.Prelude hiding (pi, Map)
import PieceOfFlake.Stm
    ( readTQueue, writeTQueue, TQueue, newTQueueIO, atomicalog )
import StmContainers.Map ( insert, listTNonAtomic, lookup, Map )
import PieceOfFlake.Stats
    ( RepoStatsF(indexedFlakes, fetchedFlakes) )

type FlakeSearchEngine = SearchEngine (FlakeUrl, MetaFlake) FlakeUrl () ()

data FlakeIndex
  = FlakeIndex
  { searchEngine :: TVar FlakeSearchEngine
  , indexerQueue :: TQueue FlakeUrl
  , indexerQueueLen :: TVar Int
  , queryCache :: TVar (LruCache Text UTCTime)
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

consumeIndexQueue :: PoF m => RepoStatsF TVar -> Map FlakeUrl Flake -> FlakeIndex -> m ()
consumeIndexQueue rs fs fi = do
  now <- liftIO getCurrentTime
  atomicalog $ do
    $(logInfo) "Wait for flakes to index for full text search"
    fu <- lift $ readTQueue fi.indexerQueue
    lift $ modifyTVar' fi.indexerQueueLen (\x -> x - 1)
    ql <- lift $ readTVar fi.indexerQueueLen
    $(logInfo) $ "Start index flake " <> show fu <> "; index queue " <> show ql
    lift (lookup fu fs) >>= \case
      Nothing -> $(logError) $ "Flake " <> show fu <> " is missing"
      Just f -> indexFlake rs now fi fs f

indexNewFlake :: (MonadLogger (t STM), MonadTrans t) => FlakeIndex -> FlakeUrl -> t STM ()
indexNewFlake fi fu = do
  iql <- lift $ do
    writeTQueue fi.indexerQueue fu
    modifyTVar' fi.indexerQueueLen (1 +)
    readTVar fi.indexerQueueLen
  $(logInfo) $ "Indexer queue increased to " <> show iql

indexFlake :: (MonadLogger (t STM), MonadTrans t) =>
  RepoStatsF TVar -> UTCTime -> FlakeIndex -> Map FlakeUrl Flake -> Flake -> t STM ()
indexFlake rs  =
  indexFlake' $ do
    lift $ do
      modifyTVar' rs.fetchedFlakes (flip (-) 1)
      modifyTVar' rs.indexedFlakes (1 +)

indexFlake' :: (MonadLogger (t STM), MonadTrans t) =>
  t STM () -> UTCTime -> FlakeIndex -> Map FlakeUrl Flake -> Flake -> t STM ()
indexFlake' onIndexed now fi fs f =
  case f of
   ff@FlakeFetched { flakeUrl } ->
     let fu = flakeUrl
         ixf = FlakeIndexed fu now ff.meta
     in
       do
         onIndexed
         lift $ do
           insert ixf fu fs
           modifyTVar' fi.searchEngine (insertDoc (fu, ff.meta))
         $(logInfo) $ "Finished index flake " <> show fu
   _nff -> $(logError) $ "Flake " <> show f.flakeUrl <> " is not in the fetched state"

loadIndexFromScratch ::
  PoF m => RepoStatsF TVar -> FlakeIndex -> Map FlakeUrl Flake -> [(FlakeUrl, Flake)] -> m ()
loadIndexFromScratch rs fi fsm fs = do
  now <- liftIO getCurrentTime
  atomicalog $ do
    $(logInfo) "Started init full text search index population"
    mapM_ (go now) fs
    $(logInfo) "Ended init full text search index population"
  where
    go now = \case
      (_, ff@FlakeFetched {}) -> indexFlake' (lift $ modifyTVar' rs.indexedFlakes (1 +)) now fi fsm ff
      (fu, nff) -> lift $ insert nff fu fsm

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
      now <- getCurrentTime
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
