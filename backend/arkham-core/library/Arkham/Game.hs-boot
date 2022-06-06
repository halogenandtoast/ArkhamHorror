module Arkham.Game where

import Arkham.Prelude
import Control.Monad.Random
import Arkham.SkillTest.Base

data Game

class HasGameRef a where
  gameRefL :: Lens' a (IORef Game)

class HasGame m where
  getGame :: m Game

class HasStdGen a where
  genL :: Lens' a (IORef StdGen)

getGameSkillTest :: HasGame m => m (Maybe SkillTest)
