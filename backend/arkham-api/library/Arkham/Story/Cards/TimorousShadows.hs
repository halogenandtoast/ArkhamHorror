module Arkham.Story.Cards.TimorousShadows (timorousShadows) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TimorousShadows = TimorousShadows StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timorousShadows :: StoryCard TimorousShadows
timorousShadows = story TimorousShadows Cards.timorousShadows

instance RunMessage TimorousShadows where
  runMessage msg s@(TimorousShadows attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    _ -> TimorousShadows <$> liftRunMessage msg attrs
