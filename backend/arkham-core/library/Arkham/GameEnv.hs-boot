module Arkham.GameEnv where

import Arkham.Prelude

import Arkham.Ability
import {-# SOURCE #-} Arkham.Game.Base
import Arkham.ActiveCost.Base
import Arkham.Classes.HasQueue
import Arkham.Distance
import Arkham.History
import Arkham.Id
import Arkham.Message
import Arkham.Modifier
import Arkham.Phase
import Arkham.Target
import Arkham.SkillTest.Base

data GameEnv

newtype GameT a = GameT { unGameT :: ReaderT GameEnv IO a }

instance Functor GameT
instance Applicative GameT
instance Monad GameT
instance MonadIO GameT
instance MonadRandom GameT
instance MonadReader GameEnv GameT

instance HasQueue Message GameT

getAllModifiers :: HasGame m => m (HashMap Target [Modifier])
getActiveAbilities :: HasGame m => m [Ability]
getPhase :: HasGame m => m Phase
getWindowDepth :: HasGame m => m Int
getDepthLock :: HasGame m => m Int
getSkillTest :: HasGame m => m (Maybe SkillTest)
getActiveCosts :: HasGame m => m [ActiveCost]
getDistance :: HasGame m => LocationId -> LocationId -> m (Maybe Distance)
getAllAbilities :: HasGame m => m [Ability]
getActionCanBeUndone :: HasGame m => m Bool
getHistory :: HasGame m => HistoryType -> InvestigatorId -> m History

class Monad m => HasGame m where
  getGame :: m Game

instance HasGame GameT

instance Monad m => HasGame (ReaderT Game m)
