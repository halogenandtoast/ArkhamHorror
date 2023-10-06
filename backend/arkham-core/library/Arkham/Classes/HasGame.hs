module Arkham.Classes.HasGame where

import {-# SOURCE #-} Arkham.Game.Base
import Arkham.Prelude

class Monad m => HasGame m where
  getGame :: m Game

instance Monad m => HasGame (ReaderT Game m) where
  getGame = ask
