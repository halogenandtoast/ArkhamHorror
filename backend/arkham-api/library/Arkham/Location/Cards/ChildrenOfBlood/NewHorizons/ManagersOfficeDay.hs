module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.ManagersOfficeDay (managersOfficeDay) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype ManagersOfficeDay = ManagersOfficeDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

managersOfficeDay :: LocationCard ManagersOfficeDay
managersOfficeDay = symbolLabel $ location ManagersOfficeDay Cards.managersOfficeDay 4 (PerPlayer 1)

instance HasAbilities ManagersOfficeDay where
  getAbilities (ManagersOfficeDay a) = extendRevealed a []

instance RunMessage ManagersOfficeDay where
  runMessage msg (ManagersOfficeDay attrs) = runQueueT $ ManagersOfficeDay <$> liftRunMessage msg attrs
