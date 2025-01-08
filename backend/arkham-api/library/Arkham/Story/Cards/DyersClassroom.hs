module Arkham.Story.Cards.DyersClassroom (dyersClassroom) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DyersClassroom = DyersClassroom StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dyersClassroom :: StoryCard DyersClassroom
dyersClassroom = story DyersClassroom Cards.dyersClassroom

instance RunMessage DyersClassroom where
  runMessage msg s@(DyersClassroom attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      handleMemory
        attrs
        Assets.professorWilliamDyerProfessorOfGeology
        Locations.dyersClassroom
        Enemies.memoryOfARegretfulVoyage
      pure s
    _ -> DyersClassroom <$> liftRunMessage msg attrs
