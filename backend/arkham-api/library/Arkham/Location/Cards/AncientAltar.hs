module Arkham.Location.Cards.AncientAltar (ancientAltar) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AncientAltar = AncientAltar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientAltar :: LocationCard AncientAltar
ancientAltar = location AncientAltar Cards.ancientAltar 4 (Static 2)

-- TODO: abilities

instance RunMessage AncientAltar where
  runMessage msg (AncientAltar attrs) = runQueueT $ AncientAltar <$> liftRunMessage msg attrs
