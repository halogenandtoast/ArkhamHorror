module Arkham.Event.Events.Lucky2 where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier

newtype Lucky2 = Lucky2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky2 :: EventCard Lucky2
lucky2 = event Lucky2 Cards.lucky2

instance RunMessage Lucky2 where
  runMessage msg e@(Lucky2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      withSkillTest \sid -> do
        drawCardsIfCan iid attrs 1
        skillTestModifier sid attrs iid $ AnySkillValue 2
        push RerunSkillTest
      pure e
    _ -> Lucky2 <$> liftRunMessage msg attrs
