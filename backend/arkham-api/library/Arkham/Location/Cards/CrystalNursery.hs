module Arkham.Location.Cards.CrystalNursery (crystalNursery) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CrystalNursery = CrystalNursery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalNursery :: LocationCard CrystalNursery
crystalNursery = symbolLabel $ location CrystalNursery Cards.crystalNursery 3 (PerPlayer 2)

instance HasAbilities CrystalNursery where
  getAbilities (CrystalNursery a) =
    extendRevealed a []

instance RunMessage CrystalNursery where
  runMessage msg (CrystalNursery attrs) = runQueueT $ case msg of
    _ -> CrystalNursery <$> liftRunMessage msg attrs
