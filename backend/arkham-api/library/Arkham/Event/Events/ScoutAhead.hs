module Arkham.Event.Events.ScoutAhead (scoutAhead) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype ScoutAhead = ScoutAhead EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scoutAhead :: EventCard ScoutAhead
scoutAhead = event ScoutAhead Cards.scoutAhead

instance RunMessage ScoutAhead where
  runMessage msg e@(ScoutAhead attrs) = runQueueT $ case msg of
    -- only the first move can provoke an attack of opportunity, so it is chosen
    -- before costs are paid; the other two are picked while resolving
    BeforePlayEvent iid eid acId | eid == toId attrs -> do
      locations <- getAccessibleLocations iid attrs
      chooseOneM iid do
        labeledI "doneMoving" nothing
        targets locations \lid -> push $ UpdateEventTarget eid (Just $ toTarget lid)
      push $ CreatedCost acId
      pure e
    PlayThisEvent iid (is attrs -> True) -> do
      cardResolutionModifier attrs attrs iid CannotBeEngaged
      for_ attrs.target \case
        LocationTarget lid -> do
          moveTo attrs iid lid
          doStep 2 msg
        _ -> pure ()
      pure e
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) | n > 0 -> do
      locations <- getAccessibleLocations iid attrs
      chooseOneM iid do
        labeledI "doneMoving" nothing
        targets locations \lid -> do
          moveTo attrs iid lid
          doStep (n - 1) msg'
      pure e
    _ -> ScoutAhead <$> liftRunMessage msg attrs
