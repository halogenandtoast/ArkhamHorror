module Arkham.Location.Cards.AbandonedWarehouse (abandonedWarehouse) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AbandonedWarehouse = AbandonedWarehouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedWarehouse :: LocationCard AbandonedWarehouse
abandonedWarehouse = location AbandonedWarehouse Cards.abandonedWarehouse 3 (PerPlayer 2)

instance RunMessage AbandonedWarehouse where
  runMessage msg (AbandonedWarehouse attrs) = AbandonedWarehouse <$> runMessage msg attrs
