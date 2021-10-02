module Arkham.Types.Event.Cards.LiveAndLearn
  ( liveAndLearn
  , LiveAndLearn(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillTest
import Arkham.Types.Target
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype LiveAndLearn = LiveAndLearn EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liveAndLearn :: EventCard LiveAndLearn
liveAndLearn = event LiveAndLearn Cards.liveAndLearn

instance EventRunner env => RunMessage env LiveAndLearn where
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
