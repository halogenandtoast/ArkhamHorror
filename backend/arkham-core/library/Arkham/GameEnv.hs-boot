module Arkham.GameEnv where

import Arkham.Prelude

import Arkham.Ability.Types
import Arkham.ActiveCost.Base
import {-# SOURCE #-} Arkham.Card
import Arkham.Classes.GameLogger
import Arkham.Classes.HasQueue
import Arkham.Distance
import {-# SOURCE #-} Arkham.Game.Base
import Arkham.History
import Arkham.Id
import Arkham.Message
import Arkham.Modifier
import Arkham.Phase
import Arkham.SkillTest.Base
import Arkham.Target

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
getJustSkillTest :: (HasGame m, HasCallStack) => m SkillTest

class Monad m => HasGame m where
  getGame :: m Game

getCard :: HasGame m => CardId -> m Card

instance HasGame GameT

instance Monad m => HasGame (ReaderT Game m)

instance CardGen GameT
instance HasGameLogger GameEnv
