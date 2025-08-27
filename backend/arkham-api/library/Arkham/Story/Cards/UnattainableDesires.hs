module Arkham.Story.Cards.UnattainableDesires (unattainableDesires) where

import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype UnattainableDesires = UnattainableDesires StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unattainableDesires :: StoryCard UnattainableDesires
unattainableDesires = story UnattainableDesires Cards.unattainableDesires

instance RunMessage UnattainableDesires where
  runMessage msg s@(UnattainableDesires attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ PlaceLocationMatching (CardWithTitle "Temple of Unattainable Desires")
      pure s
    _ -> UnattainableDesires <$> liftRunMessage msg attrs
