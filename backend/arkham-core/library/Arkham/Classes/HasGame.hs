module Arkham.Classes.HasGame where

import Arkham.Classes.HasQueue
import {-# SOURCE #-} Arkham.Game.Base
import Arkham.Prelude

class Monad m => HasGame m where
  getGame :: m Game

instance HasGame m => HasGame (QueueT msg m) where
  getGame = lift getGame

instance Monad m => HasGame (ReaderT Game m) where
  getGame = ask
