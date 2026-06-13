module Arkham.Location.Cards.DrownedAcropolisCollapsedRuins (drownedAcropolisCollapsedRuins) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DrownedAcropolisCollapsedRuins = DrownedAcropolisCollapsedRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drownedAcropolisCollapsedRuins :: LocationCard DrownedAcropolisCollapsedRuins
drownedAcropolisCollapsedRuins = location DrownedAcropolisCollapsedRuins Cards.drownedAcropolisCollapsedRuins 3 (Static 2)

-- TODO: abilities

instance RunMessage DrownedAcropolisCollapsedRuins where
  runMessage msg (DrownedAcropolisCollapsedRuins attrs) = runQueueT $ DrownedAcropolisCollapsedRuins <$> liftRunMessage msg attrs
