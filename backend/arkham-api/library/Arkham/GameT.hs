module Arkham.GameT where

import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import {-# SOURCE #-} Arkham.Game.Base
import Arkham.Message
import Arkham.Prelude
import Arkham.Queue
import Arkham.Random
import Control.Monad.Random

data GameEnv = GameEnv
  { gameEnvGame :: IORef Game
  , gameEnvQueue :: Queue Message
  , gameRandomGen :: IORef StdGen
  , gameLogger :: ClientMessage -> IO ()
  }

newtype GameT a = GameT {unGameT :: ReaderT GameEnv IO a}
  deriving newtype (MonadReader GameEnv, Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance HasGame GameT where
  getGame = asks gameEnvGame >>= readIORef

instance HasStdGen GameEnv where
  genL = lens gameRandomGen $ \m x -> m {gameRandomGen = x}

instance HasQueue Message GameT where
  messageQueue = asks gameEnvQueue

instance HasGameLogger GameT where
  getLogger = do
    logger <- asks gameLogger
    pure $ \msg -> liftIO $ logger msg

instance MonadRandom GameT where
  getRandomR lohi = do
    ref <- view genL
    atomicModifyIORef' ref (swap . randomR lohi)
  getRandom = do
    ref <- view genL
    atomicModifyIORef' ref (swap . random)
  getRandomRs lohi = do
    ref <- view genL
    gen <- atomicModifyIORef' ref split
    pure $ randomRs lohi gen
  getRandoms = do
    ref <- view genL
    gen <- atomicModifyIORef' ref split
    pure $ randoms gen
