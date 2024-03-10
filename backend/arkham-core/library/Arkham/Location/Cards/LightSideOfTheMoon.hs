module Arkham.Location.Cards.LightSideOfTheMoon (
  lightSideOfTheMoon,
  LightSideOfTheMoon (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype LightSideOfTheMoon = LightSideOfTheMoon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightSideOfTheMoon :: LocationCard LightSideOfTheMoon
lightSideOfTheMoon = location LightSideOfTheMoon Cards.lightSideOfTheMoon 0 (Static 0)

instance HasAbilities LightSideOfTheMoon where
  getAbilities (LightSideOfTheMoon attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage LightSideOfTheMoon where
  runMessage msg (LightSideOfTheMoon attrs) =
    LightSideOfTheMoon <$> runMessage msg attrs
