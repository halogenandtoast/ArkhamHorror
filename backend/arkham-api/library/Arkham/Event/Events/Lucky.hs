module Arkham.Event.Events.Lucky where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier

newtype Lucky = Lucky EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky :: EventCard Lucky
lucky = event Lucky Cards.lucky

instance RunMessage Lucky where
  runMessage msg e@(Lucky attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      withSkillTest \sid -> do
        skillTestModifier sid attrs iid (AnySkillValue 2)
        push RerunSkillTest
      pure e
    _ -> Lucky <$> liftRunMessage msg attrs
