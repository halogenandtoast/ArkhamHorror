module Arkham.Types.GameEnv where

import Arkham.Prelude

import Arkham.Types.Classes.GameLogger
import Arkham.Types.Classes.HasQueue
import Arkham.Types.Game.Classes
import Arkham.Types.Game.Types
import Arkham.Types.Message
import Control.Monad.Random.Lazy hiding (filterM, foldM, fromList)

newtype GameEnvT a = GameEnvT { unGameEnvT :: ReaderT GameEnv IO a }
  deriving newtype (MonadReader GameEnv, Functor, Applicative, Monad, MonadIO)

runGameEnvT :: (MonadIO m) => GameEnv -> GameEnvT a -> m a
runGameEnvT gameEnv = liftIO . flip runReaderT gameEnv . unGameEnvT

data GameEnv = GameEnv
  { gameEnvGame :: Game
  , gameEnvQueue :: IORef [Message]
  , gameRandomGen :: IORef StdGen
  , gameLogger :: Text -> IO ()
  }

instance HasStdGen GameEnv where
  genL = lens gameRandomGen $ \m x -> m { gameRandomGen = x }

instance HasGame GameEnv where
  gameL = lens gameEnvGame $ \m x -> m { gameEnvGame = x }

instance HasQueue GameEnv where
  messageQueue = lens gameEnvQueue $ \m x -> m { gameEnvQueue = x }

instance HasGameLogger GameEnv where
  gameLoggerL = lens gameLogger $ \m x -> m { gameLogger = x }

toGameEnv
  :: ( HasGameRef env
     , HasQueue env
     , HasStdGen env
     , HasGameLogger env
     , MonadReader env m
     , MonadIO m
     )
  => m GameEnv
toGameEnv = do
  game <- readIORef =<< view gameRefL
  gen <- view genL
  queueRef <- view messageQueue
  logger <- view gameLoggerL
  pure $ GameEnv game queueRef gen logger

instance MonadRandom GameEnvT where
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
