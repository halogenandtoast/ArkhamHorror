module Arkham.Location.Cards.ScarletHallsSanctum (scarletHallsSanctum) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype ScarletHallsSanctum = ScarletHallsSanctum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scarletHallsSanctum :: LocationCard ScarletHallsSanctum
scarletHallsSanctum = location ScarletHallsSanctum Cards.scarletHallsSanctum 0 (Static 0)

instance HasAbilities ScarletHallsSanctum where
  getAbilities (ScarletHallsSanctum a) =
    extendRevealed a []

instance RunMessage ScarletHallsSanctum where
  runMessage msg (ScarletHallsSanctum attrs) = runQueueT $ case msg of
    _ -> ScarletHallsSanctum <$> liftRunMessage msg attrs
