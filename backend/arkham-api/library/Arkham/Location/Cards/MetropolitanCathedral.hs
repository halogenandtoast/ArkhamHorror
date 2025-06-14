module Arkham.Location.Cards.MetropolitanCathedral (metropolitanCathedral) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Capability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MetropolitanCathedral = MetropolitanCathedral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

metropolitanCathedral :: LocationCard MetropolitanCathedral
metropolitanCathedral = symbolLabel $ location MetropolitanCathedral Cards.metropolitanCathedral 3 (Static 0)

instance HasAbilities MetropolitanCathedral where
  getAbilities (MetropolitanCathedral a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> youExist can.draw.cards)
          $ actionAbilityWithCost
          $ HorrorCost (a.ability 1) YouTarget 1
      , restricted a 2 (Here <> youExist (HandWith $ LengthIs $ atLeast 6)) exploreAction_
      ]

instance RunMessage MetropolitanCathedral where
  runMessage msg l@(MetropolitanCathedral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ Explore iid (attrs.ability 2) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> MetropolitanCathedral <$> liftRunMessage msg attrs
