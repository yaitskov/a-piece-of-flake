{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PieceOfFlake.Index where

import Control.Monad.Logger ( logError, logInfo, MonadLogger )
import Data.Aeson ( FromJSON )
import Data.Map.Strict qualified as M
import Data.SearchEngine
    ( SearchEngine,
      Term,
      query,
      initSearchEngine,
      insertDoc,
      FeatureFunction(LogarithmicFunction),
      SearchConfig(documentFeatureValue, SearchConfig, documentKey,
                   extractDocumentTerms, transformQueryTerm),
      SearchRankParameters(paramAutosuggestPostfilterLimit,
                           SearchRankParameters, paramK1, paramB, paramFieldWeights,
                           paramFeatureWeights, paramFeatureFunctions,
                           paramResultsetSoftLimit, paramResultsetHardLimit,
                           paramAutosuggestPrefilterLimit) )
import PieceOfFlake.Flake
    ( Flake(flakeUrl, FlakeIndexed, FlakeFetched, meta),
      FlakeUrl,
      MetaFlake(packages, description),
      PackageInfo(broken, description, license, name, unfree) )
import PieceOfFlake.Prelude hiding (pi, Map)
import PieceOfFlake.Stm ( readTQueue, TQueue, atomicalog )
import StmContainers.Map ( Map, insert, lookup )

type FlakeIndex = SearchEngine (FlakeUrl, MetaFlake) FlakeUrl () ()

packageInfoToTerms :: PackageInfo -> [Term]
packageInfoToTerms pi =
  words pi.description <>
  [ "license", pi.license
  , toText pi.name
  , if pi.unfree then "unfree" else "free"
  , if pi.broken then "broken" else "unbroken"
  ]

extractTerms :: (FlakeUrl, MetaFlake) -> () -> [Term]
extractTerms (fu, mf) () =
  maybeToList mf.description <> [toText fu] <> (toText <$> M.keys mf.packages)
  <> (concatMap packageInfoToTerms .  concatMap M.elems $ M.elems mf.packages)

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
         ixf  = FlakeIndexed fu now ff.meta
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
    $(logInfo) "Ened init full text search index population"
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

findFlakes :: MonadIO m => TVar FlakeIndex -> FlakeSearchReq -> m [ FlakeUrl ]
findFlakes fi FlakeSearchReq { searchPattern = ps } = atomicalog $ do
  $(logInfo) $ "Search flakes by " <> show ps
  r <- (`query` ps) <$> lift (readTVar fi)
  $(logInfo) $ "Found " <> show (length r) <> " by " <> show ps
  pure r
