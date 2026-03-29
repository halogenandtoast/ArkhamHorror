module Arkham.Event.Events.PhysicalFitness2 (physicalFitness2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Message.Lifted.Move

newtype PhysicalFitness2 = PhysicalFitness2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicalFitness2 :: EventCard PhysicalFitness2
physicalFitness2 = event PhysicalFitness2 Cards.physicalFitness2

instance RunMessage PhysicalFitness2 where
  runMessage msg e@(PhysicalFitness2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      locations <- getAccessibleLocations iid attrs
      chooseTargetM iid locations \lid -> do
        moveTo attrs iid lid
        healDamage iid attrs 3
      pure e
    _ -> PhysicalFitness2 <$> liftRunMessage msg attrs
