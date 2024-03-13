module Arkham.Location.Cards.CavernsBeneathTheMoonDarkSide (
  cavernsBeneathTheMoonDarkSide,
  CavernsBeneathTheMoonDarkSide (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CavernsBeneathTheMoonDarkSide = CavernsBeneathTheMoonDarkSide LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernsBeneathTheMoonDarkSide :: LocationCard CavernsBeneathTheMoonDarkSide
cavernsBeneathTheMoonDarkSide =
  locationWith
    CavernsBeneathTheMoonDarkSide
    Cards.cavernsBeneathTheMoonDarkSide
    6
    (PerPlayer 1)
    (labelL .~ "cavernsBeneathTheMoonDarkSide")

instance HasAbilities CavernsBeneathTheMoonDarkSide where
  getAbilities (CavernsBeneathTheMoonDarkSide attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage CavernsBeneathTheMoonDarkSide where
  runMessage msg (CavernsBeneathTheMoonDarkSide attrs) =
    CavernsBeneathTheMoonDarkSide <$> runMessage msg attrs
