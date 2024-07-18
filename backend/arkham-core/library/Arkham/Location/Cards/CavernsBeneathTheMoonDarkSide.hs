module Arkham.Location.Cards.CavernsBeneathTheMoonDarkSide (
  cavernsBeneathTheMoonDarkSide,
  CavernsBeneathTheMoonDarkSide (..),
)
where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

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
  getAbilities (CavernsBeneathTheMoonDarkSide x) =
    extendRevealed
      x
      [restrictedAbility x 1 Here $ forced $ SkillTestResult #after You (whileInvestigating x) #failure]

instance RunMessage CavernsBeneathTheMoonDarkSide where
  runMessage msg l@(CavernsBeneathTheMoonDarkSide attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ turnModifier iid (attrs.ability 1) attrs (ShroudModifier (-2))
      pure l
    _ -> CavernsBeneathTheMoonDarkSide <$> runMessage msg attrs
