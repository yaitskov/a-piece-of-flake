module PieceOfFlake.Prelude
  ( module X
  , duration
  ) where

import Data.Time.Clock as X
import Data.Tagged as X
import Relude as X hiding (Handle, intercalate)
import Debug.TraceEmbrace as X hiding (PackageName, Error, a)

duration :: UTCTime -> UTCTime -> Double
duration a b = realToFrac $ diffUTCTime a b
