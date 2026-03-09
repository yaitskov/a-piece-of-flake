{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
module PieceOfFlake.Stats where

import Data.RingBuffer as RB ( RingBuffer, append, new, toList )
import Data.Vector as V ( Vector, fromList )
import Generics.SOP as S
import PieceOfFlake.CmdArgs ( RingBufferSize(..) )
import PieceOfFlake.Prelude hiding (NominalDiffTime)
import PieceOfFlake.Prelude qualified as P
import PieceOfFlake.UtcTime as U ( NominalDiffTime )
import Statistics.Sample as SS ( mean )
import Text.Blaze.Internal ( MarkupM )
import Text.Blaze ( ToMarkup(toMarkup) )
import Yesod.Core ( hamlet )


class ReadTVar a b where
  readTraVar :: MonadIO m => a -> m b

greadTraVar :: (MonadIO m, S.Generic a, S.Generic b, AllZip2 ReadTVar (Code a) (Code b)) => a -> m b
greadTraVar x = to <$> greadTraVarS (from x)

greadTraVarS :: (MonadIO m, AllZip2 ReadTVar xss yss) => SOP I xss -> m (SOP I yss)
greadTraVarS (SOP (Z xs)) = SOP . Z <$> greadTraVarP xs
greadTraVarS (SOP (S xss)) = do
  SOP r <- greadTraVarS (SOP xss)
  pure $ SOP (S r)

greadTraVarP :: (MonadIO m, AllZip ReadTVar xs ys) => NP I xs -> m (NP I ys)
greadTraVarP Nil = pure Nil
greadTraVarP (I x :* xs) = do
  r <- readTraVar x
  (I r :*) <$> greadTraVarP xs



type family Columnar (f :: Type -> Type) (g :: Type -> Type) a
type instance Columnar TVar TVar a = TVar a
type instance Columnar Ydentity TVar  a = Ydentity a

type instance Columnar TVar (RingBuffer Vector) NominalDiffTime = RingBuffer Vector NominalDiffTime
type instance Columnar Ydentity (RingBuffer Vector) NominalDiffTime = Double

data RepoStatsF f
  = RepoStats
  { totalFlakeUploadsSinceRestart :: Columnar f TVar Int
  , meanFetchTime :: Columnar f (RingBuffer Vector) NominalDiffTime
  , meanIndexTime :: Columnar f (RingBuffer Vector) NominalDiffTime
  , meanTimeInFetchQueue :: Columnar f (RingBuffer Vector) NominalDiffTime
  , meanTimeInIndexQueue :: Columnar f (RingBuffer Vector) NominalDiffTime
  , submittedFlakes :: Columnar f TVar Int
  , fetchingFlakes :: Columnar f TVar Int
  , badFlakes :: Columnar f TVar Int
  , fetchedFlakes :: Columnar f TVar Int
  , indexedFlakes :: Columnar f TVar Int
  } deriving P.Generic
instance S.Generic (RepoStatsF a)

mkRepoStats :: MonadIO m => RingBufferSize -> m (RepoStatsF TVar)
mkRepoStats (unrefine . coerce -> rbs) =
  liftIO $
    RepoStats <$>
      newTVarIO 0 <*>
      RB.new rbs <*>
      RB.new rbs <*>
      RB.new rbs <*>
      RB.new rbs <*>
      newTVarIO 0 <*>
      newTVarIO 0 <*>
      newTVarIO 0 <*>
      newTVarIO 0 <*>
      newTVarIO 0

newtype Ydentity x = Ydentity { runYdentity :: x } deriving (Functor, Applicative, Monad) via Identity

instance ToMarkup a => ToMarkup (Ydentity a) where
  toMarkup = toMarkup . runYdentity

instance ReadTVar (TVar a) (Ydentity a) where
  readTraVar x = do
    r <- readTVarIO x
    pure (Ydentity r)

instance ReadTVar (RingBuffer Vector NominalDiffTime) Double where
  readTraVar = meanFetch

meanFetch :: MonadIO m => RingBuffer Vector NominalDiffTime -> m Double
meanFetch rb = SS.mean . V.fromList . fmap realToFrac <$> liftIO (RB.toList rb)

addTimeDif :: MonadIO m => RingBuffer Vector NominalDiffTime -> NominalDiffTime -> m ()
addTimeDif rb td = liftIO $ RB.append td rb

type RepoStats = RepoStatsF Ydentity

renderRepoStats :: Integer -> Int -> Tagged "fetch" Int -> RepoStatsF Ydentity -> p -> MarkupM ()
renderRepoStats searchRequests idxQueueLen (Tagged fetchQueueLen) rs =
 [hamlet|
        <h1 class="title is-4 mb-3">
          Repository stats
        <table class="table is-bordered">
          <tbody>
            <tr>
              <td>Search requests
              <td>#{searchRequests}
            <tr>
              <td>Submitted Flakes
              <td>#{rs.submittedFlakes}
            <tr>
              <td>Fetching Flakes
              <td>#{rs.fetchingFlakes}
            <tr>
              <td>Bad Flakes
              <td>#{rs.badFlakes}
            <tr>
              <td>Fetched Flakes
              <td>#{rs.fetchedFlakes}
            <tr>
              <td>Indexed Flakes
              <td>#{rs.indexedFlakes}
            <tr>
              <td>Fetch Queue
              <td>#{fetchQueueLen}
            <tr>
              <td>Index Queue
              <td>#{idxQueueLen}
            <tr>
              <td>Flake Fetch mean time
                 <td>#{rs.meanFetchTime}
            <tr>
              <td>Flake Index mean time
                 <td>#{rs.meanIndexTime}
            <tr>
              <td>Mean time flake waits in fetch queue
                 <td>#{rs.meanTimeInFetchQueue}
            <tr>
              <td>Mean time flake waits in index queue
                 <td>#{rs.meanTimeInIndexQueue}
        |]
