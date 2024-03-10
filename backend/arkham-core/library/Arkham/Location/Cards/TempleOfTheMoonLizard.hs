module Arkham.Location.Cards.TempleOfTheMoonLizard (
  templeOfTheMoonLizard,
  TempleOfTheMoonLizard (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype TempleOfTheMoonLizard = TempleOfTheMoonLizard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfTheMoonLizard :: LocationCard TempleOfTheMoonLizard
templeOfTheMoonLizard = location TempleOfTheMoonLizard Cards.templeOfTheMoonLizard 0 (Static 0)

instance HasAbilities TempleOfTheMoonLizard where
  getAbilities (TempleOfTheMoonLizard attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage TempleOfTheMoonLizard where
  runMessage msg (TempleOfTheMoonLizard attrs) =
    TempleOfTheMoonLizard <$> runMessage msg attrs
