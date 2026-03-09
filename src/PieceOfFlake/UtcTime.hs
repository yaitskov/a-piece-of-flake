module PieceOfFlake.UtcTime
  ( UtcBox
  , UTCTime
  , mkUtcBox
  , ClockMonad (..)
  , diffUTCTime
  , doAfter
  , toNominalDiffTime
  , NominalDiffTime
  , getModificationTime
  ) where

import Control.DeepSeq ( NFData(..) )
import Control.Monad.Trans ( MonadIO(liftIO), MonadTrans(..) )
import Data.Aeson ( FromJSON(parseJSON), ToJSON(toJSON) )
import Data.Coerce ( coerce )
import Data.Time.Clock qualified as C
import Data.SafeCopy
    ( SafeCopy(putCopy, getCopy), contain, safeGet, safePut )
import GHC.Conc ( STM, unsafeIOToSTM )
import GHC.Generics ( Generic )
import GHC.TypeLits ( TypeError, type (+), type (<=?), ErrorMessage(Text), Nat )
import GHC.TypeError ( Assert )
import Prelude
import UnliftIO.Directory qualified as D

newtype UTCTime (n :: Nat)
  = UTCTime
  { unUTCTime :: C.UTCTime }
  deriving newtype (Show, Eq, Ord, Generic, Read, NFData)

class Monad m => ClockMonad m where
  getCurrentTime :: m (UTCTime 0)
  getTimeAfter :: UTCTime n -> m (UTCTime (n + 1))

data UtcBox = forall n. UtcBox (UTCTime n)

mkUtcBox :: UTCTime n -> UtcBox
mkUtcBox = UtcBox

instance NFData UtcBox where
  rnf (UtcBox u) = rnf u
instance Eq UtcBox where
  (UtcBox (UTCTime a)) == (UtcBox (UTCTime b)) = a == b
instance Ord UtcBox where
  (UtcBox (UTCTime a)) `compare` (UtcBox (UTCTime b)) = a `compare` b
instance Show UtcBox where
  show (UtcBox (UTCTime ut)) = show ut
instance ToJSON UtcBox where
  toJSON (UtcBox u) = toJSON $ unUTCTime u
instance FromJSON UtcBox where
  parseJSON x = UtcBox . UTCTime <$> parseJSON x
instance SafeCopy UtcBox where
  putCopy (UtcBox (UTCTime ut)) = contain $ safePut ut
  getCopy= contain $ UtcBox . UTCTime <$> safeGet

doAfter :: ClockMonad m => UtcBox -> (forall n. (UTCTime n -> m b)) -> m b
doAfter (UtcBox u) m = m u

getModificationTime :: MonadIO m => FilePath -> m UtcBox
getModificationTime fp = UtcBox . UTCTime <$> D.getModificationTime fp

instance ClockMonad IO where
  getCurrentTime = UTCTime <$> liftIO C.getCurrentTime
  getTimeAfter _ = UTCTime <$> liftIO C.getCurrentTime

instance ClockMonad STM where
  getCurrentTime = unsafeIOToSTM getCurrentTime
  getTimeAfter x = unsafeIOToSTM (getTimeAfter x)

instance (ClockMonad m, MonadTrans t) => ClockMonad (t m) where
  getCurrentTime = lift  getCurrentTime
  getTimeAfter x = lift $ getTimeAfter x


newtype NominalDiffTime
  = NominalDiffTime
  { toNominalDiffTime :: C.NominalDiffTime
  } deriving newtype (Show, Eq, Ord, Read, Num, Enum, Fractional, Real, RealFrac, NFData)

diffUTCTime ::
  (Assert (a <=? b)
    (TypeError (Text "First argument might be less than the second"))) =>
  UTCTime b -> UTCTime a -> NominalDiffTime
diffUTCTime (coerce -> b) (coerce -> a) = NominalDiffTime $ b `C.diffUTCTime` a

_testDiff :: UTCTime 2 -> UTCTime 1 -> NominalDiffTime
_testDiff a b = a `diffUTCTime` b

_testBox :: ClockMonad m => UtcBox -> m NominalDiffTime
_testBox b =
  doAfter b (\sa -> do
                n <- getTimeAfter sa
                m <- getTimeAfter n
                pure $ n `diffUTCTime` sa + m `diffUTCTime` sa + (m `diffUTCTime` m)
            )
