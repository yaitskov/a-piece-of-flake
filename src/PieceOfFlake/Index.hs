{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PieceOfFlake.Index where

import Control.Monad.Logger ( logError, logInfo )
import Data.Aeson ( FromJSON )
import Data.Map.Strict qualified as M
import Data.SearchEngine
import PieceOfFlake.Flake
    ( FlakeUrl,
      Flake(meta, FlakeFetched, FlakeIndexed),
      MetaFlake(packages, description),
      PackageInfo(broken, description, license, name, unfree) )
import PieceOfFlake.Prelude hiding (pi, Map)
import PieceOfFlake.Stm ( readTQueue, TQueue, atomicalog )
import StmContainers.Map ( Map, insert, lookup )

type FlakeIndex = SearchEngine (FlakeUrl, MetaFlake) FlakeUrl () ()

-- textOfMetaFlake :: FlakeUrl -> MetaFlake -> Text
-- textOfMetaFlake (FlakeUrl fu) mf =

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
consumeIndexQueue fs q qlen fi = liftIO $(trIo "started/") >> go
  where
    go = do
      now <- liftIO getCurrentTime
      atomicalog $ do
        $(logInfo) "Wait for flakes to index for full text search"
        fu <- lift $ readTQueue q
        lift $ modifyTVar' qlen (\x -> x - 1)
        ql <- lift $ readTVar qlen
        $(logInfo) $ "Start index flake " <> show fu <> "; index queue " <> show ql
        lift (lookup fu fs) >>= \case
          Nothing -> $(logError) $ "Flake " <> show fu <> " is missing"
          Just ff@FlakeFetched {} -> do
            lift $ do
              let f = FlakeIndexed fu now ff.meta
              insert f fu fs
              modifyTVar' fi (insertDoc (fu, ff.meta))
            $(logInfo) $ "Finished index flake " <> show fu
          Just _nff -> $(logError) $ "Flake " <> show fu <> " is not in the fetched state"
      go

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
