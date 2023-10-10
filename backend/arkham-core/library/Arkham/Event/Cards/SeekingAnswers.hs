module Arkham.Event.Cards.SeekingAnswers (
  seekingAnswers,
  SeekingAnswers (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigate
import Arkham.Matcher

newtype SeekingAnswers = SeekingAnswers EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingAnswers :: EventCard SeekingAnswers
seekingAnswers = event SeekingAnswers Cards.seekingAnswers

instance RunMessage SeekingAnswers where
  runMessage msg e@(SeekingAnswers attrs@EventAttrs {..}) = case msg of
    PlayThisEvent iid eid | eid == eventId -> do
      pushM $ mkInvestigate iid attrs <&> setTarget attrs
      pure e
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      lids <- selectList $ ConnectedLocation <> LocationWithAnyClues
      player <- getPlayer iid
      pushIfAny lids
        $ chooseOne
          player
          [ targetLabel
            lid'
            [InvestigatorDiscoverClues iid lid' (toSource attrs) 1 (Just #investigate)]
          | lid' <- lids
          ]
      pure e
    _ -> SeekingAnswers <$> runMessage msg attrs
