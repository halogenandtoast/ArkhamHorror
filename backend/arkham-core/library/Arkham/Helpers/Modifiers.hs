module Arkham.Helpers.Modifiers where

import Arkham.Prelude

import Arkham.Source
import Arkham.Target
import Arkham.Modifier
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Classes.HasModifiersFor

getModifiers
  :: (Monad m, HasGame m) => Source
  -> Target
  -> m [ModifierType]
getModifiers source target =
  map modifierType <$> getModifiersFor source target ()
