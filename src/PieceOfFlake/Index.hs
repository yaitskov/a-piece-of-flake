{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PieceOfFlake.Index where

import Data.Aeson ( FromJSON )
import Data.Map.Strict qualified as M
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

import PieceOfFlake.Prelude hiding (pi, Map)
import PieceOfFlake.Stm ( readTQueue, TQueue, atomicalog )
import StmContainers.Map ( insert, listTNonAtomic, lookup, Map )

type FlakeIndex = SearchEngine (FlakeUrl, MetaFlake) FlakeUrl () ()

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

emptyFlakeIndex :: FlakeIndex
emptyFlakeIndex =
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

consumeIndexQueue :: MonadIO m => Map FlakeUrl Flake -> TQueue FlakeUrl -> TVar Int -> TVar FlakeIndex -> m ()
consumeIndexQueue fs q qlen fi = do
  now <- liftIO getCurrentTime
  atomicalog $ do
    $(logInfo) "Wait for flakes to index for full text search"
    fu <- lift $ readTQueue q
    lift $ modifyTVar' qlen (\x -> x - 1)
    ql <- lift $ readTVar qlen
    $(logInfo) $ "Start index flake " <> show fu <> "; index queue " <> show ql
    lift (lookup fu fs) >>= \case
      Nothing -> $(logError) $ "Flake " <> show fu <> " is missing"
      Just f -> indexFlake now fi fs f

indexFlake :: (MonadLogger (t STM), MonadTrans t) => UTCTime -> TVar FlakeIndex -> Map FlakeUrl Flake -> Flake -> t STM ()
indexFlake now fi fs f =
  case f of
   ff@FlakeFetched { flakeUrl } ->
     let fu = flakeUrl
         ixf = FlakeIndexed fu now ff.meta
     in
       do
         lift $ do
           insert ixf fu fs
           modifyTVar' fi (insertDoc (fu, ff.meta))
         $(logInfo) $ "Finished index flake " <> show fu
   _nff -> $(logError) $ "Flake " <> show f.flakeUrl <> " is not in the fetched state"

loadIndexFromScratch ::
  (MonadIO m) => TVar FlakeIndex -> Map FlakeUrl Flake -> [(FlakeUrl, Flake)] -> m ()
loadIndexFromScratch fi fsm fs = do
  now <- liftIO getCurrentTime
  atomicalog $ do
    $(logInfo) "Started init full text search index population"
    mapM_ (go now) fs
    $(logInfo) "Ended init full text search index population"
  where
    go now = \case
      (_, ff@FlakeFetched {}) -> indexFlake now fi fsm ff
      (fu, nff) -> lift $ insert nff fu fsm

data FlakeSearchReq
  = FlakeSearchReq
  { searchPattern :: [Text]
  , skipBroken :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON FlakeSearchReq

alt :: [a] -> [a] -> [a]
alt a b = case a of [] -> b ; o -> o

findFlakes :: MonadIO m => Map FlakeUrl Flake -> TVar FlakeIndex -> FlakeSearchReq -> m [ FlakeUrl ]
findFlakes fs tfi FlakeSearchReq { searchPattern = ps } =
  case concatMap tokenize ps of
    [] -> justLoadFirstNFlakes fs 30
    pst@(t1:_) -> atomicalog (fromIdx t1 pst)
  where
    fromIdx t1 pst = do
      fi <- lift $ readTVar tfi
      $(logInfo) $ "Search flakes by " <> show pst
      let r  = query fi pst `alt` (fmap fst . snd $ queryAutosuggest fi NoFilter [] t1)
      $(logInfo) $ "Found " <> show (length r) <> " by " <> show ps
      pure r

justLoadFirstNFlakes :: MonadIO m => Map FlakeUrl Flake -> Int -> m [ FlakeUrl ]
justLoadFirstNFlakes fs n =
  fmap fst . filter (isIndexed . snd) <$> liftIO (L.toList $ L.take (2 * n) (listTNonAtomic fs))
