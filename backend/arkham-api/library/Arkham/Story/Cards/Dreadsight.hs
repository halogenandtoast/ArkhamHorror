module Arkham.Story.Cards.Dreadsight (dreadsight) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype Dreadsight = Dreadsight StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreadsight :: StoryCard Dreadsight
dreadsight = story Dreadsight Cards.dreadsight

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage Dreadsight where
  runMessage msg (Dreadsight attrs) = runQueueT $ Dreadsight <$> liftRunMessage msg attrs
