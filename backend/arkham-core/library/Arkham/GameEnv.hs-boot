module Arkham.GameEnv where

import Arkham.Prelude

import Arkham.SkillTest.Base
import Arkham.Distance
import Arkham.LocationId
import Arkham.Phase

data GameEnv

newtype GameT a = GameT { unGameT :: ReaderT GameEnv IO a }

instance Functor GameT
instance Applicative GameT
instance Monad GameT
instance MonadIO GameT
instance MonadRandom GameT

getPhase :: GameT Phase
getSkillTest :: GameT (Maybe SkillTest)
getDistance :: LocationId -> LocationId -> GameT (Maybe Distance)
