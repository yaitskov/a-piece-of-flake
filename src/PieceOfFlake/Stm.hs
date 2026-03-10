module PieceOfFlake.Stm
  ( module Control.Concurrent.STM.TQueue
  , atomicalog
  ) where


import Control.Concurrent.STM.TQueue
import PieceOfFlake.Prelude hiding (Map)

atomicalog :: PoF m => WriterLoggingT STM a -> m a
atomicalog a = do
  (r, l) <- atomically (runWriterLoggingT a)
  mapM_ (unc4 monadLoggerLog) l
  pure r
  where
    unc4 f (a1, b1, c1, d1) = f a1 b1 c1 d1
