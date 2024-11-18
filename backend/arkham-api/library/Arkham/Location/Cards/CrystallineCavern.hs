module Arkham.Location.Cards.CrystallineCavern (crystallineCavern, CrystallineCavern (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CrystallineCavern = CrystallineCavern LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystallineCavern :: LocationCard CrystallineCavern
crystallineCavern = symbolLabel $ location CrystallineCavern Cards.crystallineCavern 0 (Static 0)

instance HasAbilities CrystallineCavern where
  getAbilities (CrystallineCavern attrs) =
    extendRevealed attrs []

instance RunMessage CrystallineCavern where
  runMessage msg (CrystallineCavern attrs) = runQueueT $ case msg of
    _ -> CrystallineCavern <$> liftRunMessage msg attrs
