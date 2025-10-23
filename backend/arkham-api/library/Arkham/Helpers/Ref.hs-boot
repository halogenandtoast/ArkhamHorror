module Arkham.Helpers.Ref where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Prelude
import Arkham.Source
import Arkham.Tracing

sourceToMaybeCard :: (HasCallStack, HasGame m, Tracing m , Sourceable source) => source -> m (Maybe Card)
