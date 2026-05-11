module Arkham.Location.Cards.DiningRoomHemlockHouse (diningRoomHemlockHouse) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DiningRoomHemlockHouse = DiningRoomHemlockHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diningRoomHemlockHouse :: LocationCard DiningRoomHemlockHouse
diningRoomHemlockHouse =
  locationWith DiningRoomHemlockHouse Cards.diningRoomHemlockHouse 2 (PerPlayer 3) connectsToAdjacent

instance HasAbilities DiningRoomHemlockHouse where
  getAbilities (DiningRoomHemlockHouse a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ DiscoveringLastClue #after You (be a)

instance RunMessage DiningRoomHemlockHouse where
  runMessage msg l@(DiningRoomHemlockHouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscard iid (attrs.ability 1)
      pure l
    _ -> DiningRoomHemlockHouse <$> liftRunMessage msg attrs
