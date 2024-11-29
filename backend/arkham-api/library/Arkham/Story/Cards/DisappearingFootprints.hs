module Arkham.Story.Cards.DisappearingFootprints (DisappearingFootprints (..), disappearingFootprints) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DisappearingFootprints = DisappearingFootprints StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disappearingFootprints :: StoryCard DisappearingFootprints
disappearingFootprints = story DisappearingFootprints Cards.disappearingFootprints

instance RunMessage DisappearingFootprints where
  runMessage msg s@(DisappearingFootprints attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      removeChaosToken #frost
      addToVictory attrs
      pure s
    _ -> DisappearingFootprints <$> liftRunMessage msg attrs
