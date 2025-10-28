module Arkham.Story.Cards.TheUnsealing (theUnsealing) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheUnsealing = TheUnsealing StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUnsealing :: StoryCard TheUnsealing
theUnsealing = story TheUnsealing Cards.theUnsealing

instance RunMessage TheUnsealing where
  runMessage msg s@(TheUnsealing attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> TheUnsealing <$> liftRunMessage msg attrs
