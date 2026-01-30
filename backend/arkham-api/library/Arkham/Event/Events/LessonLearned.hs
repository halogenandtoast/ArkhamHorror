module Arkham.Event.Events.LessonLearned (lessonLearned) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype LessonLearned = LessonLearned EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lessonLearned :: EventCard LessonLearned
lessonLearned = event LessonLearned Cards.lessonLearned

instance RunMessage LessonLearned where
  runMessage msg e@(LessonLearned attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discoverAtYourLocation NotInvestigate iid (toSource attrs) 1
      pure e
    _ -> LessonLearned <$> liftRunMessage msg attrs
