module Arkham.Classes.HasQueue
  ( module Arkham.Classes.HasQueue
  ) where

import Arkham.Prelude

newtype Queue msg = Queue { queueToRef :: IORef [msg] }

class MonadIO m => HasQueue msg m | m -> msg where
  messageQueue :: m (Queue msg)

newQueue :: MonadIO m => [msg] -> m (Queue msg)
newQueue msgs = Queue <$> newIORef msgs

withQueue :: HasQueue msg m => ([msg] -> ([msg], r)) -> m r
withQueue body = do
  queue <- messageQueue
  liftIO $ atomicModifyIORef' (queueToRef queue) body

withQueue_ :: HasQueue msg m => ([msg] -> [msg]) -> m ()
withQueue_ body = withQueue ((, ()) . body)

fromQueue :: HasQueue msg m => ([msg] -> r) -> m r
fromQueue f = f <$> (readIORef . queueToRef =<< messageQueue)

findFromQueue :: HasQueue msg m => (msg -> Bool) -> m (Maybe msg)
findFromQueue f = fromQueue (find f)

popMessage :: HasQueue msg m => m (Maybe msg)
popMessage = withQueue \case
  [] -> ([], Nothing)
  (m : ms) -> (ms, Just m)

clearQueue :: HasQueue msg m => m ()
clearQueue = withQueue_ $ const []

peekMessage :: HasQueue msg m => m (Maybe msg)
peekMessage = withQueue \case
  [] -> ([], Nothing)
  (m : ms) -> (m : ms, Just m)

peekQueue :: HasQueue msg m => m [msg]
peekQueue = withQueue $ \q -> (q, q)

pushEnd :: HasQueue msg m => msg -> m ()
pushEnd = pushAllEnd . pure

pushAllEnd :: HasQueue msg m => [msg] -> m ()
pushAllEnd msgs = withQueue \queue -> (queue <> msgs, ())

push :: HasQueue msg m => msg -> m ()
push = pushAll . pure

pushAll :: HasQueue msg m => [msg] -> m ()
pushAll msgs = withQueue \queue -> (msgs <> queue, ())

replaceMessage :: (HasQueue msg m, Eq msg) => msg -> [msg] -> m ()
replaceMessage msg replacement =
  replaceMessageMatching (== msg) (const replacement)

replaceMessageMatching
  :: HasQueue msg m => (msg -> Bool) -> (msg -> [msg]) -> m ()
replaceMessageMatching matcher replacer = withQueue \queue ->
  let (before, after) = break matcher queue
  in
    case after of
      [] -> (before, ())
      (msg' : rest) -> (before <> replacer msg' <> rest, ())

pushAfter :: HasQueue msg m => (msg -> Bool) -> msg -> m ()
pushAfter matcher msg = replaceMessageMatching matcher (\m -> [m, msg])

popMessageMatching
  :: HasQueue msg m => (msg -> Bool) -> m (Maybe msg)
popMessageMatching matcher = withQueue \queue ->
  let (before, after) = break matcher queue
  in
    case after of
      [] -> (before, Nothing)
      (msg' : rest) -> (before <> rest, Just msg')

popMessageMatching_ :: HasQueue msg m => (msg -> Bool) -> m ()
popMessageMatching_ = void . popMessageMatching

removeAllMessagesMatching
  :: HasQueue msg m => (msg -> Bool) -> m ()
removeAllMessagesMatching matcher = withQueue_ $ filter (not . matcher)

insertAfterMatching
  :: HasQueue msg m => [msg] -> (msg -> Bool) -> m ()
insertAfterMatching msgs p = withQueue_ \queue ->
  let (before, rest) = break p queue
  in
    case rest of
      (x : xs) -> before <> (x : msgs <> xs)
      _ -> queue
