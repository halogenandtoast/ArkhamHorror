module Arkham.Event.Events.PhysicalFitness (physicalFitness) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Message.Lifted.Move

newtype PhysicalFitness = PhysicalFitness EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

physicalFitness :: EventCard PhysicalFitness
physicalFitness = event PhysicalFitness Cards.physicalFitness

instance RunMessage PhysicalFitness where
  runMessage msg e@(PhysicalFitness attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      locations <- getAccessibleLocations iid attrs
      chooseTargetM iid locations \lid -> do
        moveTo attrs iid lid
        healDamage iid attrs 2
      pure e
    _ -> PhysicalFitness <$> liftRunMessage msg attrs
