module Arkham.Event.Cards.LiveAndLearn (
  liveAndLearn,
  LiveAndLearn (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Message
import Arkham.SkillTest
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype LiveAndLearn = LiveAndLearn EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liveAndLearn :: EventCard LiveAndLearn
liveAndLearn = event LiveAndLearn Cards.liveAndLearn

instance RunMessage LiveAndLearn where
  runMessage msg e@(LiveAndLearn attrs) = case msg of
    InvestigatorPlayEvent iid eid _ [(windowType -> Window.SkillTestEnded st)] _ | eid == toId attrs -> do
      pushAll
        [ skillTestModifier attrs (InvestigatorTarget iid) (AnySkillValue 2)
        , BeginSkillTest $
            ( buildSkillTest
                iid
                (skillTestSource st)
                (skillTestTarget st)
                (skillTestType st)
                (skillTestBaseValue st)
                (skillTestDifficulty st)
            )
              { skillTestAction = skillTestAction st
              }
        ]
      pure e
    _ -> LiveAndLearn <$> runMessage msg attrs
