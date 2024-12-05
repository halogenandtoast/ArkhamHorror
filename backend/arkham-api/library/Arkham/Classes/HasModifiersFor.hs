module Arkham.Classes.HasModifiersFor where

import Arkham.Classes.HasGame
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Target

class HasModifiersFor a where
  getModifiersFor :: (HasCallStack, HasGame m) => a -> m (Map Target [Modifier])
  getModifiersFor _ = pure mempty

instance HasModifiersFor a => HasModifiersFor (With a b) where
  getModifiersFor (With a _) = getModifiersFor a
