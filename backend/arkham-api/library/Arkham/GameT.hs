module Arkham.GameT where

import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import {-# SOURCE #-} Arkham.Game.Base
import {-# SOURCE #-} Arkham.Message
import Arkham.Prelude
import Arkham.Queue
import Arkham.Random
import Control.Monad.Random
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap

data GameEnv = GameEnv
  { gameEnvGame :: IORef Game
  , gameEnvQueue :: Queue Message
  , gameRandomGen :: IORef StdGen
  , gameLogger :: ClientMessage -> IO ()
  , gameCacheRef :: IORef (DMap CacheKey Identity)
  }

newtype GameT a = GameT {unGameT :: ReaderT GameEnv IO a}
  deriving newtype
    (MonadReader GameEnv, Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance HasGame GameT where
  getGame = asks gameEnvGame >>= readIORef
  getCache = GameCache \k build -> do
    ref <- asks gameCacheRef
    dm <- liftIO $ readIORef ref
    case DMap.lookup k dm of
      Just v -> pure $ runIdentity v
      Nothing -> do
        v <- build
        liftIO $ atomicModifyIORef' ref $ \dm' -> (DMap.insert k (Identity v) dm', ())
        pure v

instance HasStdGen GameEnv where
  genL = lens gameRandomGen $ \m x -> m {gameRandomGen = x}

instance HasQueue Message GameT where
  messageQueue = asks gameEnvQueue

instance HasGameLogger GameT where
  getLogger = do
    logger <- asks gameLogger
    pure $ \msg -> liftIO $ logger msg
