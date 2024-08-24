{-# OPTIONS_GHC -Wno-deprecations #-}

module Arkham.Debug where

import Arkham.Prelude
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock (diffUTCTime)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

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

timeIt :: Monad m => Text -> m a -> m a
timeIt label body = do
  let start = unsafePerformIO getCurrentTime
  result <- start `seq` body
  let end = unsafePerformIO getCurrentTime
  let diff = diffUTCTime end start
  let r = traceShow (T.unpack $ label <> ": " <> tshow diff) result
  r `seq` pure r
