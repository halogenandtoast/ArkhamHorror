module Arkham.Debug where

import Arkham.Prelude
import Data.Text.IO qualified as T
import System.Environment (lookupEnv)

pattern InfoLevel :: Int
pattern InfoLevel = 1

class Monad m => HasDebugLevel m where
  getDebugLevel :: m Int

instance HasDebugLevel m => HasDebugLevel (ReaderT env m) where
  getDebugLevel = lift getDebugLevel

instance HasDebugLevel IO where
  getDebugLevel = fromMaybe @Int 0 . (readMay =<<) <$> lookupEnv "DEBUG"

debugOut :: MonadIO m => Int -> Text -> m ()
debugOut n txt = liftIO do
  debugLevel <- getDebugLevel
  when (n <= debugLevel) $ T.putStrLn txt
