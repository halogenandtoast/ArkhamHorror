module Arkham.Story.Cards.AboveAndBelow (aboveAndBelow) where

import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AboveAndBelow = AboveAndBelow StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aboveAndBelow :: StoryCard AboveAndBelow
aboveAndBelow = story AboveAndBelow Cards.aboveAndBelow

instance RunMessage AboveAndBelow where
  runMessage msg s@(AboveAndBelow attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      remember InterviewedAshleigh
      pure s
    _ -> AboveAndBelow <$> liftRunMessage msg attrs
