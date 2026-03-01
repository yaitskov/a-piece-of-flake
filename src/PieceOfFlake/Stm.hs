module PieceOfFlake.Stm
  ( module Control.Concurrent.STM.TQueue
  , atomicalog
  ) where

import Data.Curry ( unc4 )
import Control.Monad.Logger
import Control.Concurrent.STM.TQueue
import PieceOfFlake.Prelude hiding (Map)

atomicalog :: MonadIO m => WriterLoggingT STM a -> m a
atomicalog a = do
  (r, l) <- atomically (runWriterLoggingT a)
  runStderrLoggingT $ do
    mapM_ (unc4 monadLoggerLog) l
  pure r
