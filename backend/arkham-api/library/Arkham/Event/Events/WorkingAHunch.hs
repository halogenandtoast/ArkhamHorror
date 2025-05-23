module Arkham.Event.Events.WorkingAHunch (workingAHunch) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype WorkingAHunch = WorkingAHunch EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

workingAHunch :: EventCard WorkingAHunch
workingAHunch = event WorkingAHunch Cards.workingAHunch

instance RunMessage WorkingAHunch where
  runMessage msg e@(WorkingAHunch attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discoverAtYourLocation NotInvestigate iid attrs 1
      pure e
    _ -> WorkingAHunch <$> liftRunMessage msg attrs
