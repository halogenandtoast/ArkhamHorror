module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.DiningHall (diningHall) where

import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted

newtype DiningHall = DiningHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diningHall :: LocationCard DiningHall
diningHall = symbolLabel $ location DiningHall Cards.diningHall 3 (PerPlayer 1)

instance HasAbilities DiningHall where
  getAbilities (DiningHall a) = extendRevealed a []

instance RunMessage DiningHall where
  runMessage msg (DiningHall attrs) = runQueueT $ DiningHall <$> liftRunMessage msg attrs
