module Arkham.Location.Cards.Xochimilco (xochimilco) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Xochimilco = Xochimilco LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

xochimilco :: LocationCard Xochimilco
xochimilco = symbolLabel $ location Xochimilco Cards.xochimilco 4 (Static 0)

instance HasModifiersFor Xochimilco where
  getModifiersFor (Xochimilco a) = modifySelect a (investigatorAt (toId a)) [CannotGainResources]

instance HasAbilities Xochimilco where
  getAbilities (Xochimilco a) = extendRevealed1 a $ restricted a 1 Here $ exploreAction $ ResourceCost 3

instance RunMessage Xochimilco where
  runMessage msg l@(Xochimilco attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Explore iid (attrs.ability 1) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> Xochimilco <$> runMessage msg attrs
