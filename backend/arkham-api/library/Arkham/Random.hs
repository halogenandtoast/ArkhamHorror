module Arkham.Random where

import Arkham.Prelude
import Control.Monad.Random (StdGen)

class HasStdGen a where
  genL :: Lens' a (IORef StdGen)
