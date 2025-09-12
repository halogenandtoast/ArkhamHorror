module Arkham.Classes.GameLogger where

import Arkham.Id
import Arkham.Prelude
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

class MonadIO m => HasGameLogger m where
  getLogger :: m (ClientMessage -> IO ())

instance HasGameLogger m => HasGameLogger (ReaderT e m) where
  getLogger = do
    logger <- lift getLogger
    pure $ \msg -> liftIO $ logger msg

instance HasGameLogger m => HasGameLogger (StateT s m) where
  getLogger = do
    logger <- lift getLogger
    pure $ \msg -> liftIO $ logger msg

instance (Monoid w, HasGameLogger m) => HasGameLogger (WriterT w m) where
  getLogger = do
    logger <- lift getLogger
    pure $ \msg -> liftIO $ logger msg

class ToGameLoggerFormat a where
  format :: a -> Text

formatAsSentence :: ToGameLoggerFormat a => [a] -> Text
formatAsSentence = go False
 where
  go _ [] = ""
  go _ [a] = format a
  go True [a, b] = format a <> ", and " <> format b
  go False [a, b] = format a <> " and " <> format b
  go _ (a : as) = format a <> ", " <> go True as

data ClientMessage
  = ClientText Text
  | ClientError Text
  | ClientCard Text Value
  | ClientCardOnly PlayerId Text Value
  | ClientTarot Value
  | ClientShowDiscard InvestigatorId
  | ClientShowUnder InvestigatorId

send :: HasGameLogger m => Text -> m ()
send msg = do
  f <- getLogger
  liftIO $ f (ClientText msg)

sendError :: HasGameLogger m => Text -> m ()
sendError msg = do
  f <- getLogger
  liftIO $ f (ClientError msg)

sendRevelation :: HasGameLogger m => PlayerId -> Value -> m ()
sendRevelation pid msg = do
  f <- getLogger
  liftIO $ f (ClientCardOnly pid "Revelation" msg)

sendReveal :: HasGameLogger m => Value -> m ()
sendReveal msg = do
  f <- getLogger
  liftIO $ f (ClientCard "Revealed" msg)

sendEnemy :: HasGameLogger m => Text -> Value -> m ()
sendEnemy title msg = do
  f <- getLogger
  liftIO $ f (ClientCard title msg)

sendTarot :: HasGameLogger m => Value -> m ()
sendTarot msg = do
  f <- getLogger
  liftIO $ f (ClientTarot msg)

sendShowDiscard :: HasGameLogger m => InvestigatorId -> m ()
sendShowDiscard iid = do
  f <- getLogger
  liftIO $ f (ClientShowDiscard iid)

sendShowUnder :: HasGameLogger m => InvestigatorId -> m ()
sendShowUnder iid = do
  f <- getLogger
  liftIO $ f (ClientShowUnder iid)

pluralize :: Int -> Text -> Text
pluralize n a = if n == 1 then "1 " <> a else tshow n <> " " <> a <> "s"

pluralize_ :: Int -> Text -> Text
pluralize_ n a = if n == 1 then a else a <> "s"

irregular :: Int -> Text -> Text -> Text
irregular n singular plural = if n == 1 then "1 " <> singular else tshow n <> " " <> plural <> "s"
