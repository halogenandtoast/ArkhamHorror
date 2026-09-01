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
    -- the destination is chosen before costs are paid, so playing this can be
    -- judged (attacks of opportunity included) knowing where you are going
    BeforePlayEvent iid eid acId | eid == toId attrs -> do
      locations <- getAccessibleLocations iid attrs
      unless (null locations) do
        chooseTargetM iid locations \lid -> push $ UpdateEventTarget eid (Just $ toTarget lid)
      push $ CreatedCost acId
      pure e
    PlayThisEvent iid (is attrs -> True) -> do
      for_ attrs.target \case
        LocationTarget lid -> do
          moveTo attrs iid lid
          healDamage iid attrs 2
        _ -> pure ()
      pure e
    _ -> PhysicalFitness <$> liftRunMessage msg attrs
