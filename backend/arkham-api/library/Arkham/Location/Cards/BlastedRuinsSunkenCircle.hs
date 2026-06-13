module Arkham.Location.Cards.BlastedRuinsSunkenCircle (blastedRuinsSunkenCircle) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype BlastedRuinsSunkenCircle = BlastedRuinsSunkenCircle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedRuinsSunkenCircle :: LocationCard BlastedRuinsSunkenCircle
blastedRuinsSunkenCircle = location BlastedRuinsSunkenCircle Cards.blastedRuinsSunkenCircle 2 (Static 1)

-- TODO: abilities

instance RunMessage BlastedRuinsSunkenCircle where
  runMessage msg (BlastedRuinsSunkenCircle attrs) = runQueueT $ BlastedRuinsSunkenCircle <$> liftRunMessage msg attrs
