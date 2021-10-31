module Arkham.Types.Game.Classes where

import Arkham.Prelude

import Arkham.Types.Game.Types
import Control.Monad.Random.Lazy hiding (filterM, foldM, fromList)

class HasGameRef a where
  gameRefL :: Lens' a (IORef Game)

class HasGame a where
  gameL :: Lens' a Game

instance HasGame Game where
  gameL = lens id $ \_ x -> x

class HasStdGen a where
  genL :: Lens' a (IORef StdGen)
