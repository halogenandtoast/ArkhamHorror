module Arkham.Event.Events.SeekingAnswers (seekingAnswers) where

import Arkham.Action qualified as Action
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
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
      locations <- select $ ConnectedLocation <> LocationWithDiscoverableCluesBy (InvestigatorWithId iid)
      chooseTargetM iid locations \lid -> discoverAt IsInvestigate iid attrs lid 1
      pure e
    _ -> SeekingAnswers <$> liftRunMessage msg attrs
