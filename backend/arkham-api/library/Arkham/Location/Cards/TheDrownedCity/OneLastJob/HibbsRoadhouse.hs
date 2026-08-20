module Arkham.Location.Cards.TheDrownedCity.OneLastJob.HibbsRoadhouse (hibbsRoadhouse) where

import Arkham.Location.CardDefs.TheDrownedCity.OneLastJob qualified as Cards
import Arkham.Location.Import.Lifted

newtype HibbsRoadhouse = HibbsRoadhouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

hibbsRoadhouse :: LocationCard HibbsRoadhouse
hibbsRoadhouse = location HibbsRoadhouse Cards.hibbsRoadhouse 3 (Static 2)

instance RunMessage HibbsRoadhouse where
  runMessage msg (HibbsRoadhouse attrs) = runQueueT $ HibbsRoadhouse <$> liftRunMessage msg attrs
