module Arkham.Helpers.Calculation where

import Arkham.Calculation
import Arkham.Classes.HasGame
import Arkham.Prelude
import Arkham.Tracing

calculate :: (HasCallStack, HasGame m, Tracing m) => GameCalculation -> m Int
