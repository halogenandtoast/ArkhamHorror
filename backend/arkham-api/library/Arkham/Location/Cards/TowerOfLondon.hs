module Arkham.Location.Cards.TowerOfLondon (towerOfLondon) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TowerOfLondon = TowerOfLondon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

towerOfLondon :: LocationCard TowerOfLondon
towerOfLondon = symbolLabel $ location TowerOfLondon Cards.towerOfLondon 3 (PerPlayer 1)

instance HasAbilities TowerOfLondon where
  getAbilities (TowerOfLondon attrs) =
    extendRevealed attrs []

instance RunMessage TowerOfLondon where
  runMessage msg (TowerOfLondon attrs) = runQueueT $ case msg of
    _ -> TowerOfLondon <$> liftRunMessage msg attrs
