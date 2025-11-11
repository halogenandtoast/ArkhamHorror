module Arkham.Story.Cards.LightsOut (lightsOut) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype LightsOut = LightsOut StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lightsOut :: StoryCard LightsOut
lightsOut = story LightsOut Cards.lightsOut

instance RunMessage LightsOut where
  runMessage msg s@(LightsOut attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> LightsOut <$> liftRunMessage msg attrs
