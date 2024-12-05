module Arkham.Location.Cards.Xochimilco (xochimilco, Xochimilco (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Xochimilco = Xochimilco LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

xochimilco :: LocationCard Xochimilco
xochimilco =
  locationWith Xochimilco Cards.xochimilco 4 (Static 0) (labelL .~ "heart")

instance HasModifiersFor Xochimilco where
  getModifiersFor (Xochimilco a) = do
    modifySelect a (investigatorAt (toId a)) [CannotGainResources]

instance HasAbilities Xochimilco where
  getAbilities (Xochimilco attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 Here $ exploreAction $ ResourceCost 3]

instance RunMessage Xochimilco where
  runMessage msg l@(Xochimilco attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      push $ Explore iid source $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> Xochimilco <$> runMessage msg attrs
