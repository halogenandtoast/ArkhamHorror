module Arkham.Classes.HasModifiersFor where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Modifier
import Arkham.Target

class HasModifiersFor a where
  getModifiersFor :: (HasCallStack, HasGame m) => Target -> a -> m [Modifier]
  getModifiersFor _ _ = pure []

instance HasModifiersFor a => HasModifiersFor (With a b) where
  getModifiersFor target (With a _) = getModifiersFor target a
