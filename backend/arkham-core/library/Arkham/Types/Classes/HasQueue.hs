module Arkham.Types.Classes.HasQueue
  ( module Arkham.Types.Classes.HasQueue
  ) where

import Arkham.Prelude

import Arkham.Types.Message

class HasQueue a where
  messageQueue :: Lens' a (IORef [Message])

withQueue
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => ([Message] -> ([Message], r))
  -> m r
withQueue body = do
  ref <- asks $ view messageQueue
  liftIO $ atomicModifyIORef' ref body

withQueue_
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => ([Message] -> [Message])
  -> m ()
withQueue_ body = withQueue ((, ()) . body)

fromQueue
  :: (MonadIO m, MonadReader env m, HasQueue env) => ([Message] -> r) -> m r
fromQueue f = f <$> (readIORef =<< asks (view messageQueue))

findFromQueue
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => (Message -> Bool)
  -> m (Maybe Message)
findFromQueue f = fromQueue (find f)

popMessage :: (MonadIO m, MonadReader env m, HasQueue env) => m (Maybe Message)
popMessage = withQueue $ \case
  [] -> ([], Nothing)
  (m : ms) -> (ms, Just m)

clearQueue :: (MonadIO m, MonadReader env m, HasQueue env) => m ()
clearQueue = withQueue $ const ([], ())

peekMessage
  :: (MonadIO m, MonadReader env m, HasQueue env) => m (Maybe Message)
peekMessage = withQueue $ \case
  [] -> ([], Nothing)
  (m : ms) -> (m : ms, Just m)

pushMessage :: (MonadIO m, MonadReader env m, HasQueue env) => Message -> m ()
pushMessage = pushMessages . pure

pushMessages
  :: (MonadIO m, MonadReader env m, HasQueue env) => [Message] -> m ()
pushMessages msgs = withQueue $ \queue -> (queue <> msgs, ())

unshiftMessage
  :: (MonadIO m, MonadReader env m, HasQueue env) => Message -> m ()
unshiftMessage = unshiftMessages . pure

unshiftMessages
  :: (MonadIO m, MonadReader env m, HasQueue env) => [Message] -> m ()
unshiftMessages msgs = withQueue $ \queue -> (msgs <> queue, ())

replaceMessage
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => Message
  -> [Message]
  -> m ()
replaceMessage msg replacement = withQueue $ \queue ->
  let (before, after) = span (== msg) queue
  in
    case after of
      [] -> (before, ())
      (_ : rest) -> (before <> replacement <> rest, ())

