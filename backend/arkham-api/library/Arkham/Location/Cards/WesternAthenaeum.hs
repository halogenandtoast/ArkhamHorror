module Arkham.Location.Cards.WesternAthenaeum (westernAthenaeum) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WesternAthenaeum = WesternAthenaeum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westernAthenaeum :: LocationCard WesternAthenaeum
westernAthenaeum = location WesternAthenaeum Cards.westernAthenaeum 5 (Static 1)

-- TODO: abilities

instance RunMessage WesternAthenaeum where
  runMessage msg (WesternAthenaeum attrs) = runQueueT $ WesternAthenaeum <$> liftRunMessage msg attrs
