module Arkham.Event.Cards.SeekingAnswers2
  ( seekingAnswers2
  , SeekingAnswers2(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType

newtype SeekingAnswers2 = SeekingAnswers2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingAnswers2 :: EventCard SeekingAnswers2
seekingAnswers2 = event SeekingAnswers2 Cards.seekingAnswers2

instance RunMessage SeekingAnswers2 where
  runMessage msg e@(SeekingAnswers2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      lid <- getJustLocation iid
      pushAll
        [ Investigate
          iid
          lid
          (toSource attrs)
          (Just $ toTarget attrs)
          SkillIntellect
          False
        , Discard (toTarget attrs)
        ]
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
      when (notNull lids) $ do
        push $ chooseOrRunOne
          iid
          [ targetLabel
              lid'
              [InvestigatorDiscoverClues iid lid' 1 (Just Action.Investigate)]
          | lid' <- lids
          ]
      pure e
    _ -> SeekingAnswers2 <$> runMessage msg attrs
