module Arkham.Story.Cards.TheFirstShow (theFirstShow) where

import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheFirstShow = TheFirstShow StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFirstShow :: StoryCard TheFirstShow
theFirstShow = story TheFirstShow Cards.theFirstShow

instance RunMessage TheFirstShow where
  runMessage msg s@(TheFirstShow attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      remember InterviewedSebastien
      pure s
    _ -> TheFirstShow <$> liftRunMessage msg attrs
