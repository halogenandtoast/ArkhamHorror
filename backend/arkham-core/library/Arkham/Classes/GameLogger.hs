module Arkham.Classes.GameLogger where

import Arkham.Prelude

class HasGameLogger a where
  gameLoggerL :: Lens' a (ClientMessage -> IO ())

class ToGameLoggerFormat a where
  format :: a -> Text

data ClientMessage = ClientText Text | ClientCard Text Value | ClientTarot Value

send :: (MonadIO m, MonadReader env m, HasGameLogger env) => Text -> m ()
send msg = do
  f <- view gameLoggerL
  liftIO $ f (ClientText msg)

sendRevelation :: (MonadIO m, MonadReader env m, HasGameLogger env) => Value -> m ()
sendRevelation msg = do
  f <- view gameLoggerL
  liftIO $ f (ClientCard "Revelation" msg)

sendEnemy :: (MonadIO m, MonadReader env m, HasGameLogger env) => Text -> Value -> m ()
sendEnemy title msg = do
  f <- view gameLoggerL
  liftIO $ f (ClientCard title msg)

sendTarot :: (MonadIO m, MonadReader env m, HasGameLogger env) => Value -> m ()
sendTarot msg = do
  f <- view gameLoggerL
  liftIO $ f (ClientTarot msg)

pluralize :: Int -> Text -> Text
pluralize n a = if n == 1 then "1 " <> a else tshow n <> " " <> a <> "s"
