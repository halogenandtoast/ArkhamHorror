module Arkham.Location.Cards.DrownedAcropolisEphemeralRuins (drownedAcropolisEphemeralRuins) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DrownedAcropolisEphemeralRuins = DrownedAcropolisEphemeralRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drownedAcropolisEphemeralRuins :: LocationCard DrownedAcropolisEphemeralRuins
drownedAcropolisEphemeralRuins = location DrownedAcropolisEphemeralRuins Cards.drownedAcropolisEphemeralRuins 3 (Static 2)

-- TODO: abilities

instance RunMessage DrownedAcropolisEphemeralRuins where
  runMessage msg (DrownedAcropolisEphemeralRuins attrs) = runQueueT $ DrownedAcropolisEphemeralRuins <$> liftRunMessage msg attrs
