module Arkham.Story.Cards.SeismicStomp (seismicStomp) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SeismicStomp = SeismicStomp StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seismicStomp :: StoryCard SeismicStomp
seismicStomp = story SeismicStomp Cards.seismicStomp

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage SeismicStomp where
  runMessage msg (SeismicStomp attrs) = runQueueT $ SeismicStomp <$> liftRunMessage msg attrs
