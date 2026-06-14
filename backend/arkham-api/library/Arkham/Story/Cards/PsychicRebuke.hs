module Arkham.Story.Cards.PsychicRebuke (psychicRebuke) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype PsychicRebuke = PsychicRebuke StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychicRebuke :: StoryCard PsychicRebuke
psychicRebuke = story PsychicRebuke Cards.psychicRebuke

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage PsychicRebuke where
  runMessage msg (PsychicRebuke attrs) = runQueueT $ PsychicRebuke <$> liftRunMessage msg attrs
