module Arkham.Classes.GameLogger where

import Arkham.Prelude

class MonadIO m => HasGameLogger m where
  getLogger :: m (ClientMessage -> IO ())

instance HasGameLogger m => HasGameLogger (ReaderT e m) where
  getLogger = do
    logger <- lift getLogger
    pure $ \msg -> liftIO $ logger msg

class ToGameLoggerFormat a where
  format :: a -> Text

data ClientMessage
  = ClientText Text
  | ClientCard Text Value
  | ClientTarot Value

send :: HasGameLogger m => Text -> m ()
send msg = do
  f <- getLogger
  liftIO $ f (ClientText msg)

sendRevelation :: HasGameLogger m => Value -> m ()
sendRevelation msg = do
  f <- getLogger
  liftIO $ f (ClientCard "Revelation" msg)

sendEnemy :: HasGameLogger m => Text -> Value -> m ()
sendEnemy title msg = do
  f <- getLogger
  liftIO $ f (ClientCard title msg)

sendTarot :: HasGameLogger m => Value -> m ()
sendTarot msg = do
  f <- getLogger
  liftIO $ f (ClientTarot msg)

pluralize :: Int -> Text -> Text
pluralize n a = if n == 1 then "1 " <> a else tshow n <> " " <> a <> "s"

irregular :: Int -> Text -> Text -> Text
irregular n singular plural = if n == 1 then "1 " <> singular else tshow n <> " " <> plural <> "s"
