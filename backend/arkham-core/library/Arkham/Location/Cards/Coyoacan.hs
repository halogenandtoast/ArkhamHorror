module Arkham.Location.Cards.Coyoacan (coyoacan, Coyoacan (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Coyoacan = Coyoacan LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

coyoacan :: LocationCard Coyoacan
coyoacan = locationWith Coyoacan Cards.coyoacan 2 (Static 0) (labelL .~ "star")

instance HasAbilities Coyoacan where
  getAbilities (Coyoacan attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here
          $ exploreAction
          $ ActionCost 1
          <> OrCost
            [ DamageCost (toSource attrs) YouTarget 1
            , HorrorCost (toSource attrs) YouTarget 1
            ]
      ]

instance RunMessage Coyoacan where
  runMessage msg l@(Coyoacan attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Explore iid (toAbilitySource attrs 1) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> Coyoacan <$> runMessage msg attrs
