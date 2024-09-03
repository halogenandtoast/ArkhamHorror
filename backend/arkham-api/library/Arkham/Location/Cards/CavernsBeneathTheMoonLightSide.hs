module Arkham.Location.Cards.CavernsBeneathTheMoonLightSide (
  cavernsBeneathTheMoonLightSide,
  CavernsBeneathTheMoonLightSide (..),
)
where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype CavernsBeneathTheMoonLightSide = CavernsBeneathTheMoonLightSide LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernsBeneathTheMoonLightSide :: LocationCard CavernsBeneathTheMoonLightSide
cavernsBeneathTheMoonLightSide =
  locationWith
    CavernsBeneathTheMoonLightSide
    Cards.cavernsBeneathTheMoonLightSide
    6
    (PerPlayer 1)
    (labelL .~ "cavernsBeneathTheMoonLightSide")

instance HasAbilities CavernsBeneathTheMoonLightSide where
  getAbilities (CavernsBeneathTheMoonLightSide x) =
    extendRevealed
      x
      [restrictedAbility x 1 Here $ forced $ SkillTestResult #after You (whileInvestigating x) #failure]

instance RunMessage CavernsBeneathTheMoonLightSide where
  runMessage msg l@(CavernsBeneathTheMoonLightSide attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ turnModifier iid (attrs.ability 1) attrs (ShroudModifier (-2))
      pure l
    _ -> CavernsBeneathTheMoonLightSide <$> runMessage msg attrs
