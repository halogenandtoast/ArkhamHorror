module Arkham.Story.Cards.UnlikelyInjuries (unlikelyInjuries) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype UnlikelyInjuries = UnlikelyInjuries StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unlikelyInjuries :: StoryCard UnlikelyInjuries
unlikelyInjuries = story UnlikelyInjuries Cards.unlikelyInjuries

instance RunMessage UnlikelyInjuries where
  runMessage msg s@(UnlikelyInjuries attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> UnlikelyInjuries <$> liftRunMessage msg attrs
