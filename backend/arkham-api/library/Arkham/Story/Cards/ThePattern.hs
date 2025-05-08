module Arkham.Story.Cards.ThePattern (thePattern) where

import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ThePattern = ThePattern StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePattern :: StoryCard ThePattern
thePattern = story ThePattern Cards.thePattern

instance RunMessage ThePattern where
  runMessage msg s@(ThePattern attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      remember InterviewedHaruko
      pure s
    _ -> ThePattern <$> liftRunMessage msg attrs
