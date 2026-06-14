module Arkham.Story.Cards.FifthEye (fifthEye) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype FifthEye = FifthEye StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fifthEye :: StoryCard FifthEye
fifthEye = story FifthEye Cards.fifthEye

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage FifthEye where
  runMessage msg (FifthEye attrs) = runQueueT $ FifthEye <$> liftRunMessage msg attrs
