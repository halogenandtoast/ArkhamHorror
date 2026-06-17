module Arkham.Event.Events.SeekingAnswers (seekingAnswers) where

import Arkham.Action qualified as Action
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Matcher

newtype SeekingAnswers = SeekingAnswers EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingAnswers :: EventCard SeekingAnswers
seekingAnswers = event SeekingAnswers Cards.seekingAnswers

instance RunMessage SeekingAnswers where
  runMessage msg e@(SeekingAnswers attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      investigateEdit_ sid iid attrs (setTarget attrs)
      pure e
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      locations <-
        select
          $ ConnectedLocation NotForMovement
          <> LocationWithDiscoverableCluesBy (InvestigatorWithId iid)
      -- Seeking Answers only redirects the base clue to a connecting location, so it
      -- is not an investigation discovery (Deduction/Rex/etc. must not pile onto it).
      chooseTargetM iid locations $ discoverAt NotInvestigate iid attrs 1
      -- "discover an additional clue at that location" effects still resolve at the
      -- investigated location (your location), per the official ruling.
      whenJustM (getLocationOf iid) \lid -> discoverAt IsInvestigate iid attrs 0 lid
      pure e
    _ -> SeekingAnswers <$> liftRunMessage msg attrs
