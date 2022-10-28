module Arkham.GameEnv where

import Arkham.Prelude

import Arkham.Ability
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

getAllModifiers :: (Monad m, HasGame m) => m (HashMap Target [Modifier])
getActiveAbilities :: (Monad m, HasGame m) => m [Ability]
getPhase :: (Monad m, HasGame m) => m Phase
getWindowDepth :: (Monad m, HasGame m) => m Int
getDepthLock :: (Monad m, HasGame m) => m Int
getSkillTest :: (Monad m, HasGame m) => m (Maybe SkillTest)
getActiveCosts :: (Monad m, HasGame m) => m [ActiveCost]
getDistance :: (Monad m, HasGame m) => LocationId -> LocationId -> m (Maybe Distance)
getAllAbilities :: (Monad m, HasGame m) => m [Ability]
getActionCanBeUndone :: (Monad m, HasGame m) => m Bool
getHistory :: (Monad m, HasGame m) => HistoryType -> InvestigatorId -> m History

class HasGame (m :: Type -> Type)

instance HasGame GameT
