module Arkham.Story.Cards.Demolition (demolition) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype Demolition = Demolition StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

demolition :: StoryCard Demolition
demolition = story Demolition Cards.demolition

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage Demolition where
  runMessage msg (Demolition attrs) = runQueueT $ Demolition <$> liftRunMessage msg attrs
