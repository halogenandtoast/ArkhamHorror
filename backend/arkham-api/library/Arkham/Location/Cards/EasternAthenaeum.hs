module Arkham.Location.Cards.EasternAthenaeum (easternAthenaeum) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype EasternAthenaeum = EasternAthenaeum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easternAthenaeum :: LocationCard EasternAthenaeum
easternAthenaeum = location EasternAthenaeum Cards.easternAthenaeum 1 (Static 1)

-- TODO: abilities

instance RunMessage EasternAthenaeum where
  runMessage msg (EasternAthenaeum attrs) = runQueueT $ EasternAthenaeum <$> liftRunMessage msg attrs
