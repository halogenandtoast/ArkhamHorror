module Arkham.Location.Cards.RelicRoomSanctumOfFortune (relicRoomSanctumOfFortune) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RelicRoomSanctumOfFortune = RelicRoomSanctumOfFortune LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicRoomSanctumOfFortune :: LocationCard RelicRoomSanctumOfFortune
relicRoomSanctumOfFortune = symbolLabel $ location RelicRoomSanctumOfFortune Cards.relicRoomSanctumOfFortune 0 (Static 0)

instance HasAbilities RelicRoomSanctumOfFortune where
  getAbilities (RelicRoomSanctumOfFortune attrs) =
    extendRevealed attrs []

instance RunMessage RelicRoomSanctumOfFortune where
  runMessage msg (RelicRoomSanctumOfFortune attrs) = runQueueT $ case msg of
    _ -> RelicRoomSanctumOfFortune <$> liftRunMessage msg attrs
