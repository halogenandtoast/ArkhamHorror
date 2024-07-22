module Arkham.Event.Cards.SeekingAnswers (seekingAnswers, SeekingAnswers (..)) where

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype SeekingAnswers = SeekingAnswers EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingAnswers :: EventCard SeekingAnswers
seekingAnswers = event SeekingAnswers Cards.seekingAnswers

instance RunMessage SeekingAnswers where
  runMessage msg e@(SeekingAnswers attrs@EventAttrs {..}) = case msg of
    PlayThisEvent iid eid | eid == eventId -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid attrs <&> setTarget attrs
      pure e
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      lids <- select $ ConnectedLocation <> LocationWithDiscoverableCluesBy (InvestigatorWithId iid)
      player <- getPlayer iid
      pushIfAny lids
        $ chooseOne
          player
          [ targetLabel lid' [Msg.DiscoverClues iid $ viaInvestigate $ discover lid' attrs 1]
          | lid' <- lids
          ]
      pure e
    _ -> SeekingAnswers <$> runMessage msg attrs
