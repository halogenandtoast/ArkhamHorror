module Arkham.Location.Cards.MoldyHallsEarlierTonight
  ( moldyHallsEarlierTonight
  , MoldyHallsEarlierTonight(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MoldyHallsEarlierTonight = MoldyHallsEarlierTonight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moldyHallsEarlierTonight :: LocationCard MoldyHallsEarlierTonight
moldyHallsEarlierTonight =
  location MoldyHallsEarlierTonight Cards.moldyHallsEarlierTonight 2 (Static 0)

instance HasAbilities MoldyHallsEarlierTonight where
  getAbilities (MoldyHallsEarlierTonight attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage MoldyHallsEarlierTonight where
  runMessage msg (MoldyHallsEarlierTonight attrs) =
    MoldyHallsEarlierTonight <$> runMessage msg attrs
