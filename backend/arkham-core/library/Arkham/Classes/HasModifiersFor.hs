module Arkham.Classes.HasModifiersFor where

import Arkham.Prelude

import Arkham.Modifier
import Arkham.Target
import {-# SOURCE #-} Arkham.GameEnv

class HasModifiersFor a where
  getModifiersFor :: (Monad m, HasGame m) => Target -> a -> m [Modifier]
  getModifiersFor _ _ = pure []

instance HasModifiersFor a => HasModifiersFor (With a b) where
  getModifiersFor target (With a _) = getModifiersFor target a
