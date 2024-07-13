module Arkham.Classes.HasQueue (
  module Arkham.Classes.HasQueue,
) where

import Arkham.Card
import Arkham.Classes.GameLogger
import Arkham.Prelude
import Control.Monad.Trans.Class
import Data.Tuple.Extra (dupe)
import Text.Pretty.Simple

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
  pure $ (a, reverse msgs)

newtype QueueT msg m a = QueueT {unQueueT :: ReaderT (Queue msg) m a}
  deriving newtype
    (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadReader (Queue msg), MonadTrans)

instance MonadIO m => HasQueue msg (QueueT msg m) where
  messageQueue = ask
  pushAll (reverse -> msgs) = withQueue_ (msgs <>)

instance HasGameLogger m => HasGameLogger (QueueT msg m) where
  getLogger = do
    logger <- lift getLogger
    pure $ \msg -> liftIO $ logger msg

instance CardGen m => CardGen (QueueT msg m) where
  genEncounterCard = lift . genEncounterCard
  genPlayerCard = lift . genPlayerCard
  replaceCard cardId card = lift $ replaceCard cardId card
  clearCardCache = lift clearCardCache

newtype Queue msg = Queue {queueToRef :: IORef [msg]}

class MonadIO m => HasQueue msg m | m -> msg where
  messageQueue :: m (Queue msg)
  pushAll :: [msg] -> m ()
  pushAll = withQueue_ . (<>)

dumpQueue :: (HasQueue msg m, Show msg) => m ()
dumpQueue = pPrint =<< readIORef . queueToRef =<< messageQueue

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
peekQueue = withQueue dupe

pushEnd :: HasQueue msg m => msg -> m ()
pushEnd = pushAllEnd . pure

pushAllEnd :: HasQueue msg m => [msg] -> m ()
pushAllEnd msgs = withQueue \queue -> (queue <> msgs, ())

push :: HasQueue msg m => msg -> m ()
push = pushAll . pure

-- pushAll :: HasQueue msg m => [msg] -> m ()
-- pushAll msgs = withQueue \queue -> (msgs <> queue, ())

replaceMessage :: (HasQueue msg m, Eq msg) => msg -> [msg] -> m ()
replaceMessage msg replacement = replaceMessageMatching (== msg) (const replacement)

mapQueue :: HasQueue msg m => (msg -> msg) -> m ()
mapQueue replacer = withQueue_ (map replacer)

replaceMessageMatching
  :: HasQueue msg m => (msg -> Bool) -> (msg -> [msg]) -> m ()
replaceMessageMatching matcher replacer = withQueue \queue ->
  let (before, after) = break matcher queue
   in case after of
        [] -> (before, ())
        (msg' : rest) -> (before <> replacer msg' <> rest, ())

replaceAllMessagesMatching
  :: HasQueue msg m => (msg -> Bool) -> (msg -> [msg]) -> m ()
replaceAllMessagesMatching matcher replacer = withQueue_ \queue ->
  flip concatMap queue \msg -> if matcher msg then replacer msg else [msg]

pushAfter :: HasQueue msg m => (msg -> Bool) -> msg -> m ()
pushAfter matcher msg = replaceMessageMatching matcher (\m -> [m, msg])

pushAllAfter :: HasQueue msg m => (msg -> Bool) -> [msg] -> m ()
pushAllAfter matcher msgs = replaceMessageMatching matcher (\m -> m : msgs)

popMessageMatching
  :: HasQueue msg m => (msg -> Bool) -> m (Maybe msg)
popMessageMatching matcher = withQueue \queue ->
  let (before, after) = break matcher queue
   in case after of
        [] -> (before, Nothing)
        (msg' : rest) -> (before <> rest, Just msg')

popMessageMatching_ :: HasQueue msg m => (msg -> Bool) -> m ()
popMessageMatching_ = void . popMessageMatching

removeAllMessagesMatching
  :: HasQueue msg m => (msg -> Bool) -> m ()
removeAllMessagesMatching matcher = withQueue_ $ filter (not . matcher)

removeAllMessagesMatchingM
  :: HasQueue msg m => (msg -> m Bool) -> m ()
removeAllMessagesMatchingM matcher = do
  queue <- peekQueue
  queue' <- filterM (fmap not . matcher) queue
  withQueue_ $ const queue'

insertAfterMatching
  :: (HasCallStack, HasQueue msg m) => [msg] -> (msg -> Bool) -> m ()
insertAfterMatching msgs p = withQueue_ \queue ->
  let (before, rest) = break p queue
   in case rest of
        (x : xs) -> before <> (x : msgs <> xs)
        _ -> error "no matching message"

assertQueue :: HasQueue msg m => (msg -> Bool) -> m Bool
assertQueue matcher = any matcher <$> peekQueue
