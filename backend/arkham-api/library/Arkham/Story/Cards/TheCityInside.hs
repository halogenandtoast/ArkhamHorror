module Arkham.Story.Cards.TheCityInside (theCityInside) where

import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheCityInside = TheCityInside StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCityInside :: StoryCard TheCityInside
theCityInside = story TheCityInside Cards.theCityInside

instance RunMessage TheCityInside where
  runMessage msg s@(TheCityInside attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ PlaceLocationMatching (CardWithTitle "City-Which-Appears-On-No-Map")
      pure s
    _ -> TheCityInside <$> liftRunMessage msg attrs
