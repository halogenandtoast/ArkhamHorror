module Arkham.Story.Cards.FindingAgentHarper
  ( FindingAgentHarper(..)
  , findingAgentHarper
  ) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype FindingAgentHarper = FindingAgentHarper StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

findingAgentHarper :: StoryCard FindingAgentHarper
findingAgentHarper = story FindingAgentHarper Cards.findingAgentHarper

instance RunMessage FindingAgentHarper where
  runMessage msg s@(FindingAgentHarper attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      pure s
    _ -> FindingAgentHarper <$> liftRunMessage msg attrs
