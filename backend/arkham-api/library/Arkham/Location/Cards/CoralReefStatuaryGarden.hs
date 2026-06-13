module Arkham.Location.Cards.CoralReefStatuaryGarden (coralReefStatuaryGarden) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CoralReefStatuaryGarden = CoralReefStatuaryGarden LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coralReefStatuaryGarden :: LocationCard CoralReefStatuaryGarden
coralReefStatuaryGarden = location CoralReefStatuaryGarden Cards.coralReefStatuaryGarden 4 (Static 3)

-- TODO: abilities

instance RunMessage CoralReefStatuaryGarden where
  runMessage msg (CoralReefStatuaryGarden attrs) = runQueueT $ CoralReefStatuaryGarden <$> liftRunMessage msg attrs
