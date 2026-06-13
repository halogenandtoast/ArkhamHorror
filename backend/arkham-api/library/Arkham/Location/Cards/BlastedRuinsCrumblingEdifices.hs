module Arkham.Location.Cards.BlastedRuinsCrumblingEdifices (blastedRuinsCrumblingEdifices) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BlastedRuinsCrumblingEdifices = BlastedRuinsCrumblingEdifices LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedRuinsCrumblingEdifices :: LocationCard BlastedRuinsCrumblingEdifices
blastedRuinsCrumblingEdifices = location BlastedRuinsCrumblingEdifices Cards.blastedRuinsCrumblingEdifices 2 (Static 1)

-- TODO: abilities

instance RunMessage BlastedRuinsCrumblingEdifices where
  runMessage msg (BlastedRuinsCrumblingEdifices attrs) = runQueueT $ BlastedRuinsCrumblingEdifices <$> liftRunMessage msg attrs
