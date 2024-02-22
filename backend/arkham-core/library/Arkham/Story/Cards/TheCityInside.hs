module Arkham.Story.Cards.TheCityInside (TheCityInside (..), theCityInside) where

import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheCityInside = TheCityInside StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCityInside :: StoryCard TheCityInside
theCityInside = story TheCityInside Cards.theCityInside

instance RunMessage TheCityInside where
  runMessage msg s@(TheCityInside attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      push $ PlaceLocationMatching (CardWithTitle "City-Which-Appears-On-No-Map")
      pure s
    _ -> TheCityInside <$> runMessage msg attrs
