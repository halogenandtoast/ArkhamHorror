module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.StorageNight (storageNight) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype StorageNight = StorageNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

storageNight :: LocationCard StorageNight
storageNight = symbolLabel $ location StorageNight Cards.storageNight 4 (PerPlayer 1)

instance HasAbilities StorageNight where
  getAbilities (StorageNight a) = extendRevealed a []

instance RunMessage StorageNight where
  runMessage msg (StorageNight attrs) = runQueueT $ StorageNight <$> liftRunMessage msg attrs
