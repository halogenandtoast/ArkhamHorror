module Arkham.Story.Cards.TheUnveiling (theUnveiling) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheUnveiling = TheUnveiling StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theUnveiling :: StoryCard TheUnveiling
theUnveiling = story TheUnveiling Cards.theUnveiling

instance RunMessage TheUnveiling where
  runMessage msg s@(TheUnveiling attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> TheUnveiling <$> liftRunMessage msg attrs
