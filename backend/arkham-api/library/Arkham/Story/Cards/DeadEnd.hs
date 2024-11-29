module Arkham.Story.Cards.DeadEnd (DeadEnd (..), deadEnd) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DeadEnd = DeadEnd StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadEnd :: StoryCard DeadEnd
deadEnd = story DeadEnd Cards.deadEnd

instance RunMessage DeadEnd where
  runMessage msg s@(DeadEnd attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      removeChaosToken #frost
      addToVictory attrs
      pure s
    _ -> DeadEnd <$> liftRunMessage msg attrs
