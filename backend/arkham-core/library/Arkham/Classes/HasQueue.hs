module Arkham.Classes.HasQueue
  ( module Arkham.Classes.HasQueue
  ) where

import Arkham.Prelude

import Arkham.Message

class HasQueue a where
  messageQueue :: Lens' a (IORef [Message])

withQueue
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => ([Message] -> ([Message], r))
  -> m r
withQueue body = do
  ref <- view messageQueue
  liftIO $ atomicModifyIORef' ref body

withQueue_
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => ([Message] -> [Message])
  -> m ()
withQueue_ body = withQueue ((, ()) . body)

fromQueue
  :: (MonadIO m, MonadReader env m, HasQueue env) => ([Message] -> r) -> m r
fromQueue f = f <$> (readIORef =<< view messageQueue)

findFromQueue
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => (Message -> Bool)
  -> m (Maybe Message)
findFromQueue f = fromQueue (find f)

popMessage :: (MonadIO m, MonadReader env m, HasQueue env) => m (Maybe Message)
popMessage = withQueue \case
  [] -> ([], Nothing)
  (m : ms) -> (ms, Just m)

clearQueue :: (MonadIO m, MonadReader env m, HasQueue env) => m ()
clearQueue = withQueue $ const ([], ())

peekMessage
  :: (MonadIO m, MonadReader env m, HasQueue env) => m (Maybe Message)
peekMessage = withQueue \case
  [] -> ([], Nothing)
  (m : ms) -> (m : ms, Just m)

pushEnd :: (MonadIO m, MonadReader env m, HasQueue env) => Message -> m ()
pushEnd = pushAllEnd . pure

pushAllEnd :: (MonadIO m, MonadReader env m, HasQueue env) => [Message] -> m ()
pushAllEnd msgs = withQueue \queue -> (queue <> msgs, ())

push :: (MonadIO m, MonadReader env m, HasQueue env) => Message -> m ()
push = pushAll . pure

pushAll :: (MonadIO m, MonadReader env m, HasQueue env) => [Message] -> m ()
pushAll msgs = withQueue \queue -> (msgs <> queue, ())

replaceMessage
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => Message
  -> [Message]
  -> m ()
replaceMessage msg replacement =
  replaceMessageMatching (== msg) (const replacement)

replaceMessageMatching
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => (Message -> Bool)
  -> (Message -> [Message])
  -> m ()
replaceMessageMatching matcher replacer = withQueue \queue ->
  let (before, after) = break matcher queue
  in
    case after of
      [] -> (before, ())
      (msg' : rest) -> (before <> replacer msg' <> rest, ())

pushAfter
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => (Message -> Bool)
  -> Message
  -> m ()
pushAfter matcher msg = replaceMessageMatching matcher (\m -> [m, msg])

popMessageMatching
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => (Message -> Bool)
  -> m (Maybe Message)
popMessageMatching matcher = withQueue \queue ->
  let (before, after) = break matcher queue
  in
    case after of
      [] -> (before, Nothing)
      (msg' : rest) -> (before <> rest, Just msg')

popMessageMatching_
  :: (MonadIO m, MonadReader env m, HasQueue env) => (Message -> Bool) -> m ()
popMessageMatching_ = void . popMessageMatching

removeAllMessagesMatching
  :: (MonadIO m, MonadReader env m, HasQueue env) => (Message -> Bool) -> m ()
removeAllMessagesMatching matcher = withQueue_ $ filter (not . matcher)

insertAfterMatching
  :: (MonadIO m, MonadReader env m, HasQueue env)
  => [Message]
  -> (Message -> Bool)
  -> m ()
insertAfterMatching msgs p = withQueue_ \queue ->
  let
    (before, rest) = break p queue
  in case rest of
    (x : xs) -> before <> (x : msgs <> xs)
    _ -> queue
