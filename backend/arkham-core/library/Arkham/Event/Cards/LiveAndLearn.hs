module Arkham.Event.Cards.LiveAndLearn
  ( liveAndLearn
  , LiveAndLearn(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Message
import Arkham.SkillTest
import Arkham.Target
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype LiveAndLearn = LiveAndLearn EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liveAndLearn :: EventCard LiveAndLearn
liveAndLearn = event LiveAndLearn Cards.liveAndLearn

instance RunMessage LiveAndLearn where
  runMessage msg e@(LiveAndLearn attrs) = case msg of
    InvestigatorPlayEvent iid eid _ [Window _ (Window.SkillTestEnded st)] _
      | eid == toId attrs -> do
        e <$ pushAll
          [ skillTestModifier attrs (InvestigatorTarget iid) (AnySkillValue 2)
          , BeginSkillTest
            iid
            (skillTestSource st)
            (skillTestTarget st)
            (skillTestAction st)
            (skillTestSkillType st)
            (skillTestDifficulty st)
          , Discard (toTarget attrs)
          ]
    _ -> LiveAndLearn <$> runMessage msg attrs
