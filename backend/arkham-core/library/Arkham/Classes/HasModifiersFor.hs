module Arkham.Classes.HasModifiersFor where

import Arkham.Prelude

import Arkham.Modifier
import Arkham.Source
import Arkham.Target

class Monad m => HasModifiersFor m a where
  getModifiersFor :: HasCallStack => Source -> Target -> a -> m [Modifier]
  getModifiersFor _ _ _ = pure []

getModifiers
  :: HasModifiersFor m ()
  => Source
  -> Target
  -> m [ModifierType]
getModifiers source target =
  map modifierType <$> getModifiersFor source target ()
