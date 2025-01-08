module Arkham.Classes.HasModifiersFor where

import Arkham.Classes.HasGame
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Target
import Control.Monad.Writer.Class
import Data.Map.Monoidal.Strict

class HasModifiersFor a where
  getModifiersFor
    :: (HasCallStack, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m) => a -> m ()
  getModifiersFor _ = pure ()

instance HasModifiersFor a => HasModifiersFor (With a b) where
  getModifiersFor (With a _) = getModifiersFor a
