module Arkham.Types.Event.Cards.SeekingAnswers
  ( seekingAnswers
  , SeekingAnswers(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype SeekingAnswers = SeekingAnswers EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingAnswers :: EventCard SeekingAnswers
seekingAnswers = event SeekingAnswers Cards.seekingAnswers

instance EventRunner env => RunMessage env SeekingAnswers where
  runMessage msg e@(SeekingAnswers attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      lid <- getId @LocationId iid
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
    SuccessfulInvestigation iid _ _ target | isTarget attrs target -> do
      lids <- selectList (ConnectedLocation <> LocationWithAnyClues)
      e <$ push
        (chooseOne
          iid
          [ TargetLabel
              (LocationTarget lid')
              [InvestigatorDiscoverClues iid lid' 1 (Just Action.Investigate)]
          | lid' <- lids
          ]
        )
    _ -> SeekingAnswers <$> runMessage msg attrs
