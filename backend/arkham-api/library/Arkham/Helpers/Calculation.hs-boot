module Arkham.Helpers.Calculation where

import Arkham.Calculation
import Arkham.Classes.HasGame
import Arkham.Prelude

calculate :: (HasCallStack, HasGame m) => GameCalculation -> m Int
