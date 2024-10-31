module Arkham.Helpers.Ref where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Prelude
import Arkham.Source

sourceToMaybeCard :: (HasCallStack, HasGame m, Sourceable source) => source -> m (Maybe Card)
