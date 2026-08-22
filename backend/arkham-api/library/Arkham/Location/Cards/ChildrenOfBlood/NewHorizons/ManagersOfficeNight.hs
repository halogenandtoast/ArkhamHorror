module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.ManagersOfficeNight (managersOfficeNight) where

import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted

newtype ManagersOfficeNight = ManagersOfficeNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

managersOfficeNight :: LocationCard ManagersOfficeNight
managersOfficeNight = symbolLabel $ location ManagersOfficeNight Cards.managersOfficeNight 4 (PerPlayer 1)

instance HasAbilities ManagersOfficeNight where
  getAbilities (ManagersOfficeNight a) = extendRevealed a []

instance RunMessage ManagersOfficeNight where
  runMessage msg (ManagersOfficeNight attrs) = runQueueT $ ManagersOfficeNight <$> liftRunMessage msg attrs
