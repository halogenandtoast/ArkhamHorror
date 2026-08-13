module Arkham.Classes.HasQueue (module Arkham.Classes.HasQueue) where

import Arkham.Prelude
import Arkham.Queue
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Tuple.Extra (dupe)

runQueueT :: HasQueue msg m => QueueT msg m a -> m a
runQueueT body = do
  inbox <- newIORef []
  a <- runReaderT (unQueueT body) (Queue inbox)
  msgs <- readIORef inbox
  pushAll $ reverse msgs
  pure a

evalQueueT :: MonadIO m => QueueT msg m a -> m [msg]
evalQueueT body = do
  inbox <- newIORef []
  _ <- runReaderT (unQueueT body) (Queue inbox)
  msgs <- readIORef inbox
  pure $ reverse msgs

execQueueT :: HasQueue msg m => QueueT msg m a -> m (a, [msg])
execQueueT body = do
  inbox <- newIORef []
  a <- runReaderT (unQueueT body) (Queue inbox)
  msgs <- readIORef inbox
  pure (a, reverse msgs)

instance MonadIO m => HasQueue msg (QueueT msg m) where
  messageQueue = ask
  pushAll (reverse -> msgs) = withQueue_ (msgs <>)

class MonadIO m => HasQueue msg m | m -> msg where
  messageQueue :: m (Queue msg)
  pushAll :: [msg] -> m ()
  pushAll = withQueue_ . (<>)

instance HasQueue msg m => HasQueue msg (StateT s m) where
  messageQueue = lift messageQueue
  pushAll = lift . pushAll

instance HasQueue msg m => HasQueue msg (ReaderT r m) where
  messageQueue = lift messageQueue
  pushAll = lift . pushAll

instance (Monoid w, HasQueue msg m) => HasQueue msg (WriterT w m) where
  messageQueue = lift messageQueue
  pushAll = lift . pushAll

{- | Transport-only wrappers a queue predicate must see through.

A wrapper that only changes /when/ or /how/ a message is delivered still leaves a
predicate written against the leaf constructor correct -- but a naive @\case Ask{} ->
True@ stops matching the moment the message is wrapped, and the resulting bug is
silent (or, for 'insertAfterMatching', a crash). Rather than teach ~150 predicates
about every wrapper, the primitives below normalize on the way in.

The policy is __normalize on the way in, strip on the way out__: a matcher is
applied to 'stripQueueWrappers' of each queued message, and any primitive that
hands the matched message back (pop, find, replace) hands back the stripped form.
Popping a wrapped message because you matched its payload means you wanted the
payload; the transport intent died with the pop.

The default instance is the identity, so a queue of some other message type is
unaffected. See the 'Arkham.Message.Message' instance for what counts as
transport-only -- it is deliberately short.
-}
class QueueWrapper msg where
  stripQueueWrappers :: msg -> msg
  stripQueueWrappers = id

-- | Apply a predicate through the transport wrappers.
matchesQueued :: QueueWrapper msg => (msg -> Bool) -> msg -> Bool
matchesQueued p = p . stripQueueWrappers

newQueue :: MonadIO m => [msg] -> m (Queue msg)
newQueue msgs = Queue <$> newIORef msgs

withQueue :: HasQueue msg m => ([msg] -> ([msg], r)) -> m r
withQueue body = do
  queue <- messageQueue
  liftIO $ atomicModifyIORef' (queueToRef queue) body

withQueue_ :: HasQueue msg m => ([msg] -> [msg]) -> m ()
withQueue_ body = withQueue ((,()) . body)

fromQueue :: HasQueue msg m => ([msg] -> r) -> m r
fromQueue f = f <$> (readIORef . queueToRef =<< messageQueue)

findFromQueue :: (HasQueue msg m, QueueWrapper msg) => (msg -> Bool) -> m (Maybe msg)
findFromQueue f = fromQueue (fmap stripQueueWrappers . find (matchesQueued f))

popMessage :: HasQueue msg m => m (Maybe msg)
popMessage = withQueue \case
  [] -> ([], Nothing)
  (m : ms) -> (ms, Just m)

clearQueue :: HasQueue msg m => m ()
clearQueue = withQueue_ $ const []

setQueue :: HasQueue msg m => [msg] -> m ()
setQueue msgs = withQueue_ $ const msgs

peekMessage :: HasQueue msg m => m (Maybe msg)
peekMessage = withQueue \case
  [] -> ([], Nothing)
  (m : ms) -> (m : ms, Just m)

peekQueue :: HasQueue msg m => m [msg]
peekQueue = withQueue dupe

pushEnd :: HasQueue msg m => msg -> m ()
pushEnd = pushAllEnd . pure

pushAllEnd :: HasQueue msg m => [msg] -> m ()
pushAllEnd msgs = withQueue \queue -> (queue <> msgs, ())

push :: HasQueue msg m => msg -> m ()
push = pushAll . pure

-- pushAll :: HasQueue msg m => [msg] -> m ()
-- pushAll msgs = withQueue \queue -> (msgs <> queue, ())

replaceMessage :: (HasQueue msg m, QueueWrapper msg, Eq msg) => msg -> [msg] -> m ()
replaceMessage msg replacement = replaceMessageMatching (== msg) (const replacement)

mapQueue :: HasQueue msg m => (msg -> msg) -> m ()
mapQueue replacer = withQueue_ (map replacer)

replaceMessageMatching
  :: (HasQueue msg m, QueueWrapper msg) => (msg -> Bool) -> (msg -> [msg]) -> m ()
replaceMessageMatching matcher replacer = withQueue \queue ->
  let (before, after) = break (matchesQueued matcher) queue
   in case after of
        [] -> (before, ())
        (msg' : rest) -> (before <> replacer (stripQueueWrappers msg') <> rest, ())

replaceMessageMatchingM
  :: (HasQueue msg m, QueueWrapper msg) => (msg -> Bool) -> (msg -> m [msg]) -> m ()
replaceMessageMatchingM matcher replacer = do
  queue <- peekQueue
  let (before, after) = break (matchesQueued matcher) queue
  case after of
    [] -> pure ()
    (msg' : rest) -> do
      msgs <- replacer (stripQueueWrappers msg')
      setQueue $ before <> msgs <> rest

replaceAllMessagesMatching
  :: (HasQueue msg m, QueueWrapper msg) => (msg -> Bool) -> (msg -> [msg]) -> m ()
replaceAllMessagesMatching matcher replacer = withQueue_ \queue ->
  flip concatMap queue \msg ->
    if matchesQueued matcher msg then replacer (stripQueueWrappers msg) else [msg]

overMessagesM :: HasQueue msg m => (msg -> m [msg]) -> m ()
overMessagesM replacer = peekQueue >>= concatMapM replacer >>= setQueue

{- | Insert @msg@ directly behind the first match, leaving the anchor as it was
found (wrappers included). A no-op when nothing matches.
-}
pushAfter :: (HasQueue msg m, QueueWrapper msg) => (msg -> Bool) -> msg -> m ()
pushAfter matcher msg = withQueue_ \queue ->
  let (before, after) = break (matchesQueued matcher) queue
   in case after of
        [] -> before
        (anchor : rest) -> before <> (anchor : msg : rest)

popMessageMatching
  :: (HasQueue msg m, QueueWrapper msg) => (msg -> Bool) -> m (Maybe msg)
popMessageMatching matcher = withQueue \queue ->
  let (before, after) = break (matchesQueued matcher) queue
   in case after of
        [] -> (before, Nothing)
        (msg' : rest) -> (before <> rest, Just (stripQueueWrappers msg'))

popMessagesMatching :: (HasQueue msg m, QueueWrapper msg) => (msg -> Bool) -> m [msg]
popMessagesMatching f = withQueue \queue ->
  let go acc [] = acc
      go (a, b) (msg : rest) =
        if matchesQueued f msg
          then go (a, b <> [stripQueueWrappers msg]) rest
          else go (a <> [msg], b) rest
   in go ([], []) queue

popMessageMatching_ :: (HasQueue msg m, QueueWrapper msg) => (msg -> Bool) -> m ()
popMessageMatching_ = void . popMessageMatching

removeAllMessagesMatching
  :: (HasQueue msg m, QueueWrapper msg) => (msg -> Bool) -> m ()
removeAllMessagesMatching matcher = withQueue_ $ filter (not . matchesQueued matcher)

removeAllMessagesMatchingM
  :: (HasQueue msg m, QueueWrapper msg) => (msg -> m Bool) -> m ()
removeAllMessagesMatchingM matcher = do
  queue <- peekQueue
  queue' <- filterM (fmap not . matcher . stripQueueWrappers) queue
  withQueue_ $ const queue'

insertAfterMatching
  :: (HasCallStack, HasQueue msg m, QueueWrapper msg) => [msg] -> (msg -> Bool) -> m ()
insertAfterMatching msgs p = withQueue_ \queue ->
  let (before, rest) = break (matchesQueued p) queue
   in case rest of
        (x : xs) -> before <> (x : msgs <> xs)
        _ -> error $ "no matching message:\n" <> prettyCallStack callStack

insertAfterMatchingOrNow
  :: (HasQueue msg m, QueueWrapper msg) => [msg] -> (msg -> Bool) -> m ()
insertAfterMatchingOrNow msgs p = do
  result <- withQueue \queue ->
    let (before, rest) = break (matchesQueued p) queue
     in case rest of
          (x : xs) -> (before <> (x : msgs <> xs), True)
          _ -> (queue, False)
  unless result $ pushAll msgs

assertQueue :: (HasQueue msg m, QueueWrapper msg) => (msg -> Bool) -> m Bool
assertQueue matcher = any (matchesQueued matcher) <$> peekQueue
