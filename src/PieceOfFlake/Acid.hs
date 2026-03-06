{-# LANGUAGE TypeFamilies #-}
module PieceOfFlake.Acid where

import Data.Acid
    ( query,
      update,
      openLocalStateFrom,
      makeAcidic,
      AcidState,
      Query,
      Update )
import PieceOfFlake.CmdArgs ( AcidFlakesPath )
import Data.SafeCopy ( deriveSafeCopy, base )
import PieceOfFlake.Flake ( FlakeUrl, Flake )
import PieceOfFlake.Prelude
import PieceOfFlake.Stm ( TQueue, readTQueue, peekTQueue )
import UnliftIO.Retry
    ( fibonacciBackoff, limitRetries, recoverAll )


newtype AcidFlakes = AcidFlakes { unAcidFlakes :: [(FlakeUrl, Flake)] }

$(deriveSafeCopy 0 'base ''AcidFlakes)

type Message = (FlakeUrl, Flake)
-- Transactions are defined to run in either the 'Update' monad
-- or the 'Query' monad.
addMessage :: Message -> Update AcidFlakes ()
addMessage msg
    = do AcidFlakes messages <- get
         put $ AcidFlakes (msg:messages)

viewMessages :: Query AcidFlakes [Message]
viewMessages = unAcidFlakes <$> ask

-- This will define @ViewMessage@ and @AddMessage@ for us.
$(makeAcidic ''AcidFlakes ['addMessage, 'viewMessages])

openFlakeDb :: MonadIO m => Tagged AcidFlakesPath FilePath -> m (AcidState AcidFlakes)
openFlakeDb (Tagged dbLocation) =
  liftIO $ openLocalStateFrom dbLocation (AcidFlakes [])

updateDb :: PoF m => AcidState AcidFlakes -> Message -> m ()
updateDb db m = do
  liftIO $ update db (AddMessage m)
  $(logInfo) $ "Persisted flake " <> show (fst m)

loadFromDb :: MonadIO m => AcidState AcidFlakes -> m [(FlakeUrl, Flake)]
loadFromDb db = liftIO (query db ViewMessages)

runPersistQueue :: PoF m => AcidState AcidFlakes -> TQueue (FlakeUrl, Flake) -> m ()
runPersistQueue db q = finally go (void $ atomically $ readTQueue q)
  where
    go =
      recoverAll
        (fibonacciBackoff 100_000 <> limitRetries 3)
        (\_ -> updateDb db =<< atomically (peekTQueue q))
