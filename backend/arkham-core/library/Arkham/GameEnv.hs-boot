module Arkham.GameEnv where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes.HasQueue
import Arkham.Distance
import Arkham.LocationId
import Arkham.Phase
import Arkham.SkillTest.Base

data GameEnv

newtype GameT a = GameT { unGameT :: ReaderT GameEnv IO a }

instance Functor GameT
instance Applicative GameT
instance Monad GameT
instance MonadIO GameT
instance MonadRandom GameT
instance MonadReader GameEnv GameT

instance HasQueue GameEnv

getPhase :: (Monad m, HasGame m) => m Phase
getWindowDepth :: (Monad m, HasGame m) => m Int
getDepthLock :: (Monad m, HasGame m) => m Int
getSkillTest :: (Monad m, HasGame m) => m (Maybe SkillTest)
getDistance :: (Monad m, HasGame m) => LocationId -> LocationId -> m (Maybe Distance)
getAllAbilities :: (Monad m, HasGame m) => m [Ability]

class HasGame (m :: Type -> Type)

instance HasGame GameT
