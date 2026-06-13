module Arkham.Location.Cards.GreatLiftInactive (greatLiftInactive) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype GreatLiftInactive = GreatLiftInactive LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greatLiftInactive :: LocationCard GreatLiftInactive
greatLiftInactive = location GreatLiftInactive Cards.greatLiftInactive 2 (Static 1)

-- TODO: abilities

instance RunMessage GreatLiftInactive where
  runMessage msg (GreatLiftInactive attrs) = runQueueT $ GreatLiftInactive <$> liftRunMessage msg attrs
