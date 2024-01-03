module Arkham.Story.Cards.GhostsOfTheDead (GhostsOfTheDead (..), ghostsOfTheDead) where

import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype GhostsOfTheDead = GhostsOfTheDead StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghostsOfTheDead :: StoryCard GhostsOfTheDead
ghostsOfTheDead = story GhostsOfTheDead Cards.ghostsOfTheDead

instance RunMessage GhostsOfTheDead where
  runMessage msg s@(GhostsOfTheDead attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      push $ Remember KnowWhatHappenedToIb
      pure s
    _ -> GhostsOfTheDead <$> runMessage msg attrs
