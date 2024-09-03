module Arkham.Story.Cards.UnattainableDesires (UnattainableDesires (..), unattainableDesires) where

import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype UnattainableDesires = UnattainableDesires StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unattainableDesires :: StoryCard UnattainableDesires
unattainableDesires = story UnattainableDesires Cards.unattainableDesires

instance RunMessage UnattainableDesires where
  runMessage msg s@(UnattainableDesires attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      push $ PlaceLocationMatching (CardWithTitle "Temple of Unattainable Desires")
      pure s
    _ -> UnattainableDesires <$> runMessage msg attrs
