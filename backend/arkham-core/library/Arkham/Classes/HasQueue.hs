module Arkham.Classes.HasQueue
  ( module Arkham.Classes.HasQueue
  ) where

import Arkham.Prelude

class HasQueue msg m where
  messageQueue :: m (IORef [msg])

withQueue
  :: (MonadIO m, HasQueue msg m)
  => ([msg] -> ([msg], r))
  -> m r
withQueue body = do
  ref <- messageQueue
  liftIO $ atomicModifyIORef' ref body

withQueue_
  :: (MonadIO m, HasQueue msg m)
  => ([msg] -> [msg])
  -> m ()
withQueue_ body = withQueue ((, ()) . body)

fromQueue
  :: (MonadIO m, HasQueue msg m) => ([msg] -> r) -> m r
fromQueue f = f <$> (readIORef =<< messageQueue)

findFromQueue
  :: (MonadIO m, HasQueue msg m)
  => (msg -> Bool)
  -> m (Maybe msg)
findFromQueue f = fromQueue (find f)

popMessage :: forall msg m. (MonadIO m, HasQueue msg m) => m (Maybe msg)
popMessage = withQueue \case
  [] -> ([], Nothing)
  (m : ms) -> (ms, Just m)

clearQueue :: forall msg m. (MonadIO m, HasQueue msg m) => m ()
clearQueue = withQueue_ @_ @msg $ const []

peekMessage
  :: (MonadIO m, HasQueue msg m) => m (Maybe msg)
peekMessage = withQueue \case
  [] -> ([], Nothing)
  (m : ms) -> (m : ms, Just m)

peekQueue
  :: forall msg m. (MonadIO m, HasQueue msg m) => m [msg]
peekQueue = withQueue $ \q -> (q, q)

pushEnd :: (MonadIO m, HasQueue msg m) => msg -> m ()
pushEnd = pushAllEnd . pure

pushAllEnd :: (MonadIO m, HasQueue msg m) => [msg] -> m ()
pushAllEnd msgs = withQueue \queue -> (queue <> msgs, ())

push :: (MonadIO m, HasQueue msg m) => msg -> m ()
push = pushAll . pure

pushAll :: (MonadIO m, HasQueue msg m) => [msg] -> m ()
pushAll msgs = withQueue \queue -> (msgs <> queue, ())

replaceMessage
  :: (MonadIO m, HasQueue msg m, Eq msg)
  => msg
  -> [msg]
  -> m ()
replaceMessage msg replacement =
  replaceMessageMatching (== msg) (const replacement)

replaceMessageMatching
  :: (MonadIO m, HasQueue msg m)
  => (msg -> Bool)
  -> (msg -> [msg])
  -> m ()
replaceMessageMatching matcher replacer = withQueue \queue ->
  let (before, after) = break matcher queue
  in
    case after of
      [] -> (before, ())
      (msg' : rest) -> (before <> replacer msg' <> rest, ())

pushAfter
  :: (MonadIO m, HasQueue msg m)
  => (msg -> Bool)
  -> msg
  -> m ()
pushAfter matcher msg = replaceMessageMatching matcher (\m -> [m, msg])

popMessageMatching
  :: (MonadIO m, HasQueue msg m)
  => (msg -> Bool)
  -> m (Maybe msg)
popMessageMatching matcher = withQueue \queue ->
  let (before, after) = break matcher queue
  in
    case after of
      [] -> (before, Nothing)
      (msg' : rest) -> (before <> rest, Just msg')

popMessageMatching_
  :: (MonadIO m, HasQueue msg m) => (msg -> Bool) -> m ()
popMessageMatching_ = void . popMessageMatching

removeAllMessagesMatching
  :: (MonadIO m, HasQueue msg m) => (msg -> Bool) -> m ()
removeAllMessagesMatching matcher = withQueue_ $ filter (not . matcher)

insertAfterMatching
  :: (MonadIO m, HasQueue msg m)
  => [msg]
  -> (msg -> Bool)
  -> m ()
insertAfterMatching msgs p = withQueue_ \queue ->
  let
    (before, rest) = break p queue
  in case rest of
    (x : xs) -> before <> (x : msgs <> xs)
    _ -> queue
