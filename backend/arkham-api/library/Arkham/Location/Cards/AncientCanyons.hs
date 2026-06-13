module Arkham.Location.Cards.AncientCanyons (ancientCanyons) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AncientCanyons = AncientCanyons LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientCanyons :: LocationCard AncientCanyons
ancientCanyons = location AncientCanyons Cards.ancientCanyons 3 (Static 1)

-- TODO: abilities

instance RunMessage AncientCanyons where
  runMessage msg (AncientCanyons attrs) = runQueueT $ AncientCanyons <$> liftRunMessage msg attrs
