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
          healDamage iid attrs 3
        _ -> pure ()
      pure e
    _ -> PhysicalFitness2 <$> liftRunMessage msg attrs
