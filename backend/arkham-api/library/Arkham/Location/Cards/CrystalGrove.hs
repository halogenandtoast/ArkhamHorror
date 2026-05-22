module Arkham.Location.Cards.CrystalGrove (crystalGrove) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CrystalGrove = CrystalGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalGrove :: LocationCard CrystalGrove
crystalGrove = symbolLabel $ locationWith CrystalGrove Cards.crystalGrove 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities CrystalGrove where
  getAbilities (CrystalGrove a) =
    extendRevealed a []

instance RunMessage CrystalGrove where
  runMessage msg (CrystalGrove attrs) = runQueueT $ case msg of
    _ -> CrystalGrove <$> liftRunMessage msg attrs
