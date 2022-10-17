module Arkham.Location.Cards.TimeWrackedWoods
  ( timeWrackedWoods
  , TimeWrackedWoods(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TimeWrackedWoods = TimeWrackedWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeWrackedWoods :: LocationCard TimeWrackedWoods
timeWrackedWoods =
  location TimeWrackedWoods Cards.timeWrackedWoods 4 (PerPlayer 2)

instance HasAbilities TimeWrackedWoods where
  getAbilities (TimeWrackedWoods attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage TimeWrackedWoods where
  runMessage msg (TimeWrackedWoods attrs) =
    TimeWrackedWoods <$> runMessage msg attrs
