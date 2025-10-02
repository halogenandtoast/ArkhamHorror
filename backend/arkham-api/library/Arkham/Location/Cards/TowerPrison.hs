module Arkham.Location.Cards.TowerPrison (towerPrison) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TowerPrison = TowerPrison LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

towerPrison :: LocationCard TowerPrison
towerPrison = symbolLabel $ location TowerPrison Cards.towerPrison 4 (PerPlayer 2)

instance HasAbilities TowerPrison where
  getAbilities (TowerPrison attrs) =
    extendRevealed attrs []

instance RunMessage TowerPrison where
  runMessage msg (TowerPrison attrs) = runQueueT $ case msg of
    _ -> TowerPrison <$> liftRunMessage msg attrs
