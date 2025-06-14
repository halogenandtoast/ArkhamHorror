module Arkham.Location.Cards.Coyoacan (coyoacan) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Coyoacan = Coyoacan LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coyoacan :: LocationCard Coyoacan
coyoacan = symbolLabel $ location Coyoacan Cards.coyoacan 2 (Static 0)

instance HasAbilities Coyoacan where
  getAbilities (Coyoacan a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ exploreAction
      $ OrCost [DamageCost (a.ability 1) YouTarget 1, HorrorCost (a.ability 1) YouTarget 1]

instance RunMessage Coyoacan where
  runMessage msg l@(Coyoacan attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Explore iid (attrs.ability 1) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> Coyoacan <$> liftRunMessage msg attrs
