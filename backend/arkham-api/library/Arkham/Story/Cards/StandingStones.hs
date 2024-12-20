module Arkham.Story.Cards.StandingStones (standingStones) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype StandingStones = StandingStones StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

standingStones :: StoryCard StandingStones
standingStones = story StandingStones Cards.standingStones

instance RunMessage StandingStones where
  runMessage msg s@(StandingStones attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> StandingStones <$> liftRunMessage msg attrs
