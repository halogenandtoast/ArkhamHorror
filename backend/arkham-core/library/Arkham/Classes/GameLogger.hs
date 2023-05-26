module Arkham.Classes.GameLogger where

import Arkham.Prelude

class HasGameLogger a where
  gameLoggerL :: Lens' a (Text -> IO ())

class ToGameLoggerFormat a where
  format :: a -> Text

send :: (MonadIO m, MonadReader env m, HasGameLogger env) => Text -> m ()
send msg = do
  f <- view gameLoggerL
  liftIO $ f msg

pluralize :: Int -> Text -> Text
pluralize n a = if n == 1 then "1 " <> a else tshow n <> " " <> a <> "s"
