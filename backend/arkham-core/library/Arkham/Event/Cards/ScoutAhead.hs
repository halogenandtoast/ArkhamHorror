module Arkham.Event.Cards.ScoutAhead (scoutAhead, ScoutAhead (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Modifier
import Arkham.Movement

newtype ScoutAhead = ScoutAhead EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scoutAhead :: EventCard ScoutAhead
scoutAhead = event ScoutAhead Cards.scoutAhead

instance RunMessage ScoutAhead where
  runMessage msg e@(ScoutAhead attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      cardResolutionModifier attrs attrs iid CannotBeEngaged
      push $ DoStep 3 msg
      pure e
    DoStep n msg'@(PlayThisEvent iid eid) | eid == toId attrs && n > 0 -> do
      locations <- getAccessibleLocations iid attrs
      chooseOne iid
        $ Label "Done Moving" []
        : [targetLabel lid [Move $ move attrs iid lid, DoStep (n - 1) msg'] | lid <- locations]
      pure e
    _ -> ScoutAhead <$> liftRunMessage msg attrs
