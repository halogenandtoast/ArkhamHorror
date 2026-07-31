module Arkham.Event.Events.CleanSweep (cleanSweep) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Helpers.SkillTest (getSkillTestTargetedLocation)
import Arkham.Helpers.SkillTest.Lifted (investigate_)
import Arkham.Location.Types (Field (..))
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Projection

newtype CleanSweep = CleanSweep EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cleanSweep :: EventCard CleanSweep
cleanSweep = event CleanSweep Cards.cleanSweep

instance RunMessage CleanSweep where
  runMessage msg e@(CleanSweep attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (SkillModifier #intellect 2)
      investigate_ sid iid attrs
      pure e
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      runMaybeT_ do
        lid <- MaybeT getSkillTestTargetedLocation
        clues <- lift $ field LocationClues lid
        guard (clues == 1)
        lift $ skillTestCardOption attrs $ doStep 1 msg
      pure e
    -- The accessible locations must be gathered when the option resolves, not when it is
    -- registered: taking the last clue can unblock a connecting location (e.g. Passenger
    -- Car #169, blocked while the location to its left has clues), and the discovery only
    -- happens once the skill test results are being applied.
    DoStep 1 (PassedThisSkillTest iid (isSource attrs -> True)) -> do
      locations <- getAccessibleLocations iid attrs
      unless (null locations) do
        chooseOneM iid do
          labeledI "doNotMove" nothing
          labeledI "moveToConnecting" do
            chooseTargetM iid locations $ moveTo attrs iid
      pure e
    _ -> CleanSweep <$> liftRunMessage msg attrs
