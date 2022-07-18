module Arkham.GameEnv where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes.HasAbilities
import Arkham.Phase
import Arkham.Classes.GameLogger
import Arkham.Classes.HasQueue
import Arkham.Classes.HasDistance
import Arkham.Distance
import {-# SOURCE #-} Arkham.Game
import Arkham.History
import Arkham.Id
import Arkham.Message
import Arkham.SkillTest.Base
import Control.Monad.Random.Lazy hiding ( filterM, foldM, fromList )

newtype GameT a = GameT {unGameT :: ReaderT GameEnv IO a}
  deriving newtype (MonadReader GameEnv, Functor, Applicative, Monad, MonadIO)

class HasGame m where
  getGame :: m Game

instance Monad m => HasGame (ReaderT Game m) where
  getGame = ask

runGameEnvT :: MonadIO m => GameEnv -> GameT a -> m a
runGameEnvT gameEnv = liftIO . flip runReaderT gameEnv . unGameT

data GameEnv = GameEnv
  { gameEnvGame :: Game
  , gameEnvQueue :: IORef [Message]
  , gameRandomGen :: IORef StdGen
  , gameLogger :: Text -> IO ()
  }

instance HasStdGen GameEnv where
  genL = lens gameRandomGen $ \m x -> m { gameRandomGen = x }

instance HasGame GameT where
  getGame = asks $ gameEnvGame

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

getSkillTest :: (Monad m, HasGame m) => m (Maybe SkillTest)
getSkillTest = gameSkillTest <$> getGame

getHistory :: (Monad m, HasGame m) => HistoryType -> InvestigatorId -> m History
getHistory TurnHistory iid =
  findWithDefault mempty iid . gameTurnHistory <$> getGame
getHistory PhaseHistory iid =
  findWithDefault mempty iid . gamePhaseHistory <$> getGame
getHistory RoundHistory iid = do
  roundH <- findWithDefault mempty iid . gameRoundHistory <$> getGame
  phaseH <- getHistory PhaseHistory iid
  pure $ roundH <> phaseH

getDistance :: (Monad m, HasGame m) => LocationId -> LocationId -> m (Maybe Distance)
getDistance l1 l2 = do
  game <- getGame
  getDistance' game l1 l2

getPhase :: (Monad m, HasGame m) => m Phase
getPhase = gamePhase <$> getGame

getWindowDepth :: (Monad m, HasGame m) => m Int
getWindowDepth = gameWindowDepth <$> getGame

getDepthLock :: (Monad m, HasGame m) => m Int
getDepthLock = gameDepthLock <$> getGame

getAllAbilities :: (Monad m, HasGame m) => m [Ability]
getAllAbilities = getAbilities <$> getGame

getActionCanBeUndone :: (Monad m, HasGame m) => m Bool
getActionCanBeUndone = gameActionCanBeUndone <$> getGame
