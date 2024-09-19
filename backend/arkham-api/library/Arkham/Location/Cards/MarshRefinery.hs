module Arkham.Location.Cards.MarshRefinery (marshRefinery, MarshRefinery (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MarshRefinery = MarshRefinery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marshRefinery :: LocationCard MarshRefinery
marshRefinery = location MarshRefinery Cards.marshRefinery 1 (PerPlayer 1)

instance HasAbilities MarshRefinery where
  getAbilities (MarshRefinery attrs) =
    extendRevealed
      attrs
      [playerLimit PerGame $ restrictedAbility attrs 1 (Here <> can.draw.cards You) doubleActionAbility]

instance RunMessage MarshRefinery where
  runMessage msg l@(MarshRefinery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 4
      pure l
    _ -> MarshRefinery <$> liftRunMessage msg attrs
