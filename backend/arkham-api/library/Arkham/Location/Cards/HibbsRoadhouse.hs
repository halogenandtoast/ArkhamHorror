module Arkham.Location.Cards.HibbsRoadhouse (hibbsRoadhouse) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype HibbsRoadhouse = HibbsRoadhouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hibbsRoadhouse :: LocationCard HibbsRoadhouse
hibbsRoadhouse = location HibbsRoadhouse Cards.hibbsRoadhouse 3 (Static 2)

instance RunMessage HibbsRoadhouse where
  runMessage msg (HibbsRoadhouse attrs) = runQueueT $ HibbsRoadhouse <$> liftRunMessage msg attrs
