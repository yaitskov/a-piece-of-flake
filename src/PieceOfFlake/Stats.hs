{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PieceOfFlake.Stats where

import Generics.SOP as S
import PieceOfFlake.Prelude
import PieceOfFlake.Prelude qualified as P
import Yesod.Core ( hamlet )
import Text.Blaze.Internal ( MarkupM )
import Text.Blaze ( ToMarkup(toMarkup) )


class ReadTVar a b where
  readTraVar :: a -> STM b

greadTraVar :: (S.Generic a, S.Generic b, AllZip2 ReadTVar (Code a) (Code b)) => a -> STM b
greadTraVar x = to <$> greadTraVarS (from x)

greadTraVarS :: (AllZip2 ReadTVar xss yss) => SOP I xss -> STM (SOP I yss)
greadTraVarS (SOP (Z xs)) = SOP . Z <$> greadTraVarP xs
greadTraVarS (SOP (S xss)) = do
  SOP r <- greadTraVarS (SOP xss)
  pure $ SOP (S r)

greadTraVarP :: (AllZip ReadTVar xs ys) => NP I xs -> STM (NP I ys)
greadTraVarP Nil = pure Nil
greadTraVarP (I x :* xs) = do
  r <- readTraVar x
  (I r :*) <$> greadTraVarP xs


data RepoStatsF f
  = RepoStats
  { totalFlakeUploadsSinceRestart :: f Int
  , submittedFlakes :: f Int
  , fetchingFlakes :: f Int
  , badFlakes :: f Int
  , fetchedFlakes :: f Int
  , indexedFlakes :: f Int
  } deriving P.Generic
instance S.Generic (RepoStatsF a)

mkRepoStats :: MonadIO m => m (RepoStatsF TVar)
mkRepoStats =
  liftIO $
    RepoStats <$>
      newTVarIO 0 <*>
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
    r <- readTVar x
    pure (Ydentity r)

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
        |]
