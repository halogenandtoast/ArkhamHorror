module Arkham.Location.Cards.GrowingFields (growingFields) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GrowingFields = GrowingFields LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

growingFields :: LocationCard GrowingFields
growingFields = location GrowingFields Cards.growingFields 3 (Static 1)

-- TODO: abilities

instance RunMessage GrowingFields where
  runMessage msg (GrowingFields attrs) = runQueueT $ GrowingFields <$> liftRunMessage msg attrs
