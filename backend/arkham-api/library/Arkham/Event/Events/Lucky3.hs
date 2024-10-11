module Arkham.Event.Events.Lucky3 where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Modifier
import Arkham.SkillTest.Base

newtype Lucky3 = Lucky3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky3 :: EventCard Lucky3
lucky3 = event Lucky3 Cards.lucky3

instance RunMessage Lucky3 where
  runMessage msg e@(Lucky3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      getSkillTest >>= \case
        Nothing -> error "invalid call"
        Just skillTest -> do
          let iid' = skillTestInvestigator skillTest
          drawCardsIfCan iid attrs 1
          skillTestModifier skillTest.id attrs iid' (AnySkillValue 3)
          push RerunSkillTest
      pure e
    _ -> Lucky3 <$> liftRunMessage msg attrs
