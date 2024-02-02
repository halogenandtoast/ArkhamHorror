module Arkham.Location.Cards.TempleRuins (templeRuins, TempleRuins (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype TempleRuins = TempleRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

templeRuins :: LocationCard TempleRuins
templeRuins = locationWith TempleRuins Cards.templeRuins 4 (Static 0) (labelL .~ "circle")

instance HasAbilities TempleRuins where
  getAbilities (TempleRuins attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here
          $ ActionAbility [#explore]
          $ ActionCost 2
      ]

instance RunMessage TempleRuins where
  runMessage msg l@(TempleRuins attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      push $ Explore iid source $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> TempleRuins <$> runMessage msg attrs
