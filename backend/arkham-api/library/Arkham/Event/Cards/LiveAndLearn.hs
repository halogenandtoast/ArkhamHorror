module Arkham.Event.Cards.LiveAndLearn (liveAndLearn, LiveAndLearn (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype LiveAndLearn = LiveAndLearn EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liveAndLearn :: EventCard LiveAndLearn
liveAndLearn = event LiveAndLearn Cards.liveAndLearn

instance RunMessage LiveAndLearn where
  runMessage msg e@(LiveAndLearn attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ [windowType -> Window.SkillTestEnded st] _ | eid == toId attrs -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AnySkillValue 2)
      push $ RepeatSkillTest sid st
      pure e
    InvestigatorPlayEvent _ eid _ windows' _ | eid == toId attrs -> do
      error $ "Wrong windows: " <> show windows'
    _ -> LiveAndLearn <$> liftRunMessage msg attrs
