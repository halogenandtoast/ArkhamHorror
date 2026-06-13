module Arkham.Location.Cards.GreatLiftActive (greatLiftActive) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GreatLiftActive = GreatLiftActive LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greatLiftActive :: LocationCard GreatLiftActive
greatLiftActive = location GreatLiftActive Cards.greatLiftActive 2 (Static 0)

-- TODO: abilities

instance RunMessage GreatLiftActive where
  runMessage msg (GreatLiftActive attrs) = runQueueT $ GreatLiftActive <$> liftRunMessage msg attrs
