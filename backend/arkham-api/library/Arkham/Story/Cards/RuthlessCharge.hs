module Arkham.Story.Cards.RuthlessCharge (ruthlessCharge) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype RuthlessCharge = RuthlessCharge StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruthlessCharge :: StoryCard RuthlessCharge
ruthlessCharge = story RuthlessCharge Cards.ruthlessCharge

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage RuthlessCharge where
  runMessage msg (RuthlessCharge attrs) = runQueueT $ RuthlessCharge <$> liftRunMessage msg attrs
