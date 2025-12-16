module Arkham.Location.Cards.CongressChamberSanctum (congressChamberSanctum) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CongressChamberSanctum = CongressChamberSanctum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congressChamberSanctum :: LocationCard CongressChamberSanctum
congressChamberSanctum = location CongressChamberSanctum Cards.congressChamberSanctum 0 (Static 0)

instance HasAbilities CongressChamberSanctum where
  getAbilities (CongressChamberSanctum a) =
    extendRevealed a []

instance RunMessage CongressChamberSanctum where
  runMessage msg (CongressChamberSanctum attrs) = runQueueT $ case msg of
    _ -> CongressChamberSanctum <$> liftRunMessage msg attrs
