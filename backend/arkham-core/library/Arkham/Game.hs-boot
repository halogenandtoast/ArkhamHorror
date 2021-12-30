module Arkham.Game where

import Arkham.Prelude
import Control.Monad.Random

data Game

class HasGameRef a where
  gameRefL :: Lens' a (IORef Game)

class HasGame a where
  gameL :: Lens' a Game

class HasStdGen a where
  genL :: Lens' a (IORef StdGen)
