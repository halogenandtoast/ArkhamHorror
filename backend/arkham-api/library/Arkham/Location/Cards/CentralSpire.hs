module Arkham.Location.Cards.CentralSpire (centralSpire) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CentralSpire = CentralSpire LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

centralSpire :: LocationCard CentralSpire
centralSpire = location CentralSpire Cards.centralSpire 3 (Static 2)

-- TODO: abilities

instance RunMessage CentralSpire where
  runMessage msg (CentralSpire attrs) = runQueueT $ CentralSpire <$> liftRunMessage msg attrs
