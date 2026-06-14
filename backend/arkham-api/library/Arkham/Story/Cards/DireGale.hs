module Arkham.Story.Cards.DireGale (direGale) where

import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DireGale = DireGale StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

direGale :: StoryCard DireGale
direGale = story DireGale Cards.direGale

-- TODO: behavior for the Cthulhu deck / storm-deck card (no engine support yet).
instance RunMessage DireGale where
  runMessage msg (DireGale attrs) = runQueueT $ DireGale <$> liftRunMessage msg attrs
