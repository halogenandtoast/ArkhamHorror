module Arkham.Event.Events.LessonLearned2 (lessonLearned2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype LessonLearned2 = LessonLearned2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lessonLearned2 :: EventCard LessonLearned2
lessonLearned2 = event LessonLearned2 Cards.lessonLearned2

instance RunMessage LessonLearned2 where
  runMessage msg e@(LessonLearned2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discoverAtYourLocation NotInvestigate iid (toSource attrs) 2
      pure e
    _ -> LessonLearned2 <$> liftRunMessage msg attrs
