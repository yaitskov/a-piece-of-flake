module PieceOfFlake.Index where

import Data.SearchEngine
import PieceOfFlake.Prelude


type FlakeIndex = SearchEngine Text Text () ()

extractTerms :: Text -> () -> [Term]
extractTerms doc () = words doc

emptyFlakeIndex :: FlakeIndex
emptyFlakeIndex =
  initSearchEngine
    SearchConfig
    { documentKey = id
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
