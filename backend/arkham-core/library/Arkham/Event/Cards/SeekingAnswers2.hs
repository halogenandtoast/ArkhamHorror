module Arkham.Event.Cards.SeekingAnswers2 (
  seekingAnswers2,
  SeekingAnswers2 (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigate
import Arkham.Matcher

newtype SeekingAnswers2 = SeekingAnswers2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

seekingAnswers2 :: EventCard SeekingAnswers2
seekingAnswers2 = event SeekingAnswers2 Cards.seekingAnswers2

instance RunMessage SeekingAnswers2 where
  runMessage msg e@(SeekingAnswers2 attrs@EventAttrs {..}) = case msg of
    PlayThisEvent iid eid | eid == eventId -> do
      pushM $ mkInvestigate iid attrs <&> setTarget attrs
      pure e
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      pushAll
        [ ResolveEvent iid (toId attrs) Nothing []
        , ResolveEvent iid (toId attrs) Nothing []
        ]
      pure e
    ResolveEvent iid eid _ _ | eid == toId attrs -> do
      lids <-
        selectList
          $ LocationMatchAny [locationWithInvestigator iid, ConnectedLocation]
          <> locationWithDiscoverableCluesBy iid
      player <- getPlayer iid
      pushIfAny lids
        $ chooseOrRunOne player
        $ [ targetLabel lid'
            $ [InvestigatorDiscoverClues iid lid' (toSource attrs) 1 (Just #investigate)]
          | lid' <- lids
          ]
      pure e
    _ -> SeekingAnswers2 <$> runMessage msg attrs
