module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.StorageDay (storageDay) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype StorageDay = StorageDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

storageDay :: LocationCard StorageDay
storageDay = symbolLabel $ location StorageDay Cards.storageDay 4 (PerPlayer 1)

instance HasAbilities StorageDay where
  getAbilities (StorageDay a) = extendRevealed a []

instance RunMessage StorageDay where
  runMessage msg (StorageDay attrs) = runQueueT $ StorageDay <$> liftRunMessage msg attrs
