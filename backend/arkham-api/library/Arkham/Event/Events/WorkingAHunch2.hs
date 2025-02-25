module Arkham.Event.Events.WorkingAHunch2 where

import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype WorkingAHunch2 = WorkingAHunch2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

workingAHunch2 :: EventCard WorkingAHunch2
workingAHunch2 = event WorkingAHunch2 Cards.workingAHunch2

instance RunMessage WorkingAHunch2 where
  runMessage msg e@(WorkingAHunch2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      locations <- select $ RevealedLocation <> locationWithDiscoverableCluesBy iid
      chooseTargetM iid locations \lid ->
        discoverAt NotInvestigate iid attrs lid 1
      pure e
    _ -> WorkingAHunch2 <$> liftRunMessage msg attrs
