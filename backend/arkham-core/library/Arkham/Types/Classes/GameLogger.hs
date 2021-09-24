module Arkham.Types.Classes.GameLogger where

import Arkham.Prelude

class HasGameLogger a where
  gameLoggerL :: Lens' a (Text -> IO ())

send :: (MonadIO m, MonadReader env m, HasGameLogger env) => Text -> m ()
send msg = do
  f <- view gameLoggerL
  liftIO $ f msg
