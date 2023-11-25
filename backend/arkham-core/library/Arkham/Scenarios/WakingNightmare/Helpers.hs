module Arkham.Scenarios.WakingNightmare.Helpers where

import Arkham.GameValue
import Arkham.Matcher

pattern InfestedLocation :: LocationMatcher
pattern InfestedLocation <- (LocationWithDamage (GreaterThan (Static 0)))
  where
    InfestedLocation = LocationWithDamage (GreaterThan (Static 0))
