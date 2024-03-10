module Arkham.Location.Cards.CavernsBeneathTheMoonLightSide
  ( cavernsBeneathTheMoonLightSide
  , CavernsBeneathTheMoonLightSide(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CavernsBeneathTheMoonLightSide = CavernsBeneathTheMoonLightSide LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernsBeneathTheMoonLightSide :: LocationCard CavernsBeneathTheMoonLightSide
cavernsBeneathTheMoonLightSide = location CavernsBeneathTheMoonLightSide Cards.cavernsBeneathTheMoonLightSide 0 (Static 0)

instance HasAbilities CavernsBeneathTheMoonLightSide where
  getAbilities (CavernsBeneathTheMoonLightSide attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage CavernsBeneathTheMoonLightSide where
  runMessage msg (CavernsBeneathTheMoonLightSide attrs) =
    CavernsBeneathTheMoonLightSide <$> runMessage msg attrs
