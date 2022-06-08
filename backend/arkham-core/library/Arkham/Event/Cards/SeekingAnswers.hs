module Arkham.Event.Cards.SeekingAnswers
  ( seekingAnswers
  , SeekingAnswers(..)
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

newtype SeekingAnswers = SeekingAnswers EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingAnswers :: EventCard SeekingAnswers
seekingAnswers = event SeekingAnswers Cards.seekingAnswers

instance RunMessage SeekingAnswers where
  runMessage msg e@(SeekingAnswers attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      lid <- getJustLocation iid
      e <$ pushAll
        [ Investigate
          iid
          lid
          (toSource attrs)
          (Just $ toTarget attrs)
          SkillIntellect
          False
        , Discard (toTarget attrs)
        ]
    Successful (Action.Investigate, _) iid _ target _ | isTarget attrs target ->
      do
        lids <- selectList (ConnectedLocation <> LocationWithAnyClues)
        e <$ push
          (chooseOne
            iid
            [ targetLabel
                lid'
                [InvestigatorDiscoverClues iid lid' 1 (Just Action.Investigate)]
            | lid' <- lids
            ]
          )
    _ -> SeekingAnswers <$> runMessage msg attrs
