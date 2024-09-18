module Arkham.Classes.HasGame where

import {-# SOURCE #-} Arkham.Game.Base
import Arkham.Prelude
import Arkham.Queue
import Control.Monad.State

class Monad m => HasGame m where
  getGame :: m Game

instance HasGame m => HasGame (QueueT msg m) where
  getGame = lift getGame

instance Monad m => HasGame (ReaderT Game m) where
  getGame = ask

instance HasGame m => HasGame (StateT s m) where
  getGame = lift getGame
