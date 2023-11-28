module Arkham.Location.Cards.MetropolitanCathedral (metropolitanCathedral, MetropolitanCathedral (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype MetropolitanCathedral = MetropolitanCathedral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

metropolitanCathedral :: LocationCard MetropolitanCathedral
metropolitanCathedral = locationWith MetropolitanCathedral Cards.metropolitanCathedral 3 (Static 0) (labelL .~ "square")

instance HasAbilities MetropolitanCathedral where
  getAbilities (MetropolitanCathedral attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility attrs 1 Here
            $ actionAbilityWithCost
            $ HorrorCost (toSource attrs) YouTarget 1
        , restrictedAbility
            attrs
            2
            (Here <> exists (You <> HandWith (LengthIs $ atLeast 6)))
            exploreAction_
        ]

instance RunMessage MetropolitanCathedral where
  runMessage msg l@(MetropolitanCathedral attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = toAbilitySource attrs 2
      push $ Explore iid source $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> MetropolitanCathedral <$> runMessage msg attrs
