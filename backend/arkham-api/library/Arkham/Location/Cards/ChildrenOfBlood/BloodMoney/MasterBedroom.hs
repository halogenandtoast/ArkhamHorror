module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.MasterBedroom (masterBedroom) where

import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted

newtype MasterBedroom = MasterBedroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

masterBedroom :: LocationCard MasterBedroom
masterBedroom = symbolLabel $ location MasterBedroom Cards.masterBedroom 4 (PerPlayer 2)

instance HasAbilities MasterBedroom where
  getAbilities (MasterBedroom a) = extendRevealed a []

instance RunMessage MasterBedroom where
  runMessage msg (MasterBedroom attrs) = runQueueT $ MasterBedroom <$> liftRunMessage msg attrs
