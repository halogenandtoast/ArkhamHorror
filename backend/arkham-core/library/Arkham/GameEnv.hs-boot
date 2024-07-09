module Arkham.GameEnv where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.ActiveCost.Base
import {-# SOURCE #-} Arkham.Card (Card, CardCode, CardId)
import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.RunMessage.Internal
import Arkham.Distance
import {-# SOURCE #-} Arkham.Game
import Arkham.Game.Settings
import Arkham.History
import Arkham.Id
import {-# SOURCE #-} Arkham.Message
import Arkham.Modifier
import Arkham.Phase
import Arkham.SkillTest.Base
import Arkham.Target
import Arkham.Window
import Control.Monad.Random

withModifiers'
  :: (Targetable target, HasGame m)
  => target
  -> [Modifier]
  -> (forall t. (MonadTrans t, HasGame (t m)) => t m a)
  -> m a
getAllModifiers :: HasGame m => m (Map Target [Modifier])
getActiveAbilities :: HasGame m => m [Ability]
getPhase :: HasGame m => m Phase
getCurrentBatchId :: HasGame m => m (Maybe BatchId)
getWindowDepth :: HasGame m => m Int
getDepthLock :: HasGame m => m Int
getSkillTest :: HasGame m => m (Maybe SkillTest)
getActiveCosts :: HasGame m => m [ActiveCost]
getDistance :: HasGame m => LocationId -> LocationId -> m (Maybe Distance)
getAllAbilities :: HasGame m => m [Ability]
getWindowStack :: HasGame m => m [[Window]]
getActionCanBeUndone :: HasGame m => m Bool
getGameInAction :: HasGame m => m Bool
getIgnoreCanModifiers :: HasGame m => m Bool
getHistory :: HasGame m => HistoryType -> InvestigatorId -> m History
getHistoryField :: HasGame m => HistoryType -> InvestigatorId -> HistoryField k -> m k
getJustSkillTest :: (HasGame m, HasCallStack) => m SkillTest
getCard :: HasGame m => CardId -> m Card
findCard :: HasGame m => (Card -> Bool) -> m (Maybe Card)
getSettings :: HasGame m => m Settings
getAllPlayers :: HasGame m => m [PlayerId]
getActivePlayer :: HasGame m => m PlayerId
getCardUses :: HasGame m => CardCode -> m Int

data GameEnv = GameEnv
  { gameEnvGame :: IORef Game
  , gameEnvQueue :: Queue Message
  , gameRandomGen :: IORef StdGen
  , gameLogger :: ClientMessage -> IO ()
  }

newtype GameT a = GameT {unGameT :: ReaderT GameEnv IO a}

instance Functor GameT
instance Applicative GameT
instance Monad GameT

runWithEnv
  :: ( HasGameRef env
     , HasQueue Message m
     , HasStdGen env
     , HasGameLogger m
     , MonadReader env m
     )
  => GameT a
  -> m a

instance HasGame GameT
instance MonadRandom GameT
instance CanRun GameT
instance MonadUnliftIO GameT
