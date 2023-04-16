module Arkham.Location.Cards.MoldyHalls
  ( moldyHalls
  , MoldyHalls(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MoldyHalls = MoldyHalls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moldyHalls :: LocationCard MoldyHalls
moldyHalls = location MoldyHalls Cards.moldyHalls 4 (PerPlayer 1)

instance HasAbilities MoldyHalls where
  getAbilities (MoldyHalls attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage MoldyHalls where
  runMessage msg (MoldyHalls attrs) = MoldyHalls <$> runMessage msg attrs
