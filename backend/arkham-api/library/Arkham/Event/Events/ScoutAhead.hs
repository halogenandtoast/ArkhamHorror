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
    PlayThisEvent iid (is attrs -> True) -> do
      cardResolutionModifier attrs attrs iid CannotBeEngaged
      doStep 3 msg
      pure e
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) | n > 0 -> do
      locations <- getAccessibleLocations iid attrs
      chooseOneM iid do
        labeled "Done Moving" nothing
        targets locations \lid -> do
          moveTo attrs iid lid
          doStep (n - 1) msg'
      pure e
    ResolvedCard iid card | attrs.cardId == card.id -> do
      push $ CheckEnemyEngagement iid
      pure e
    _ -> ScoutAhead <$> liftRunMessage msg attrs
