module Arkham.GameEnv where

import Arkham.Prelude

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

getPhase :: GameT Phase
getWindowDepth :: GameT Int
getSkillTest :: GameT (Maybe SkillTest)
getDistance :: LocationId -> LocationId -> GameT (Maybe Distance)
