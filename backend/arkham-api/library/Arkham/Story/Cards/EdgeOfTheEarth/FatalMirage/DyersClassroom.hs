module Arkham.Story.Cards.EdgeOfTheEarth.FatalMirage.DyersClassroom (dyersClassroom) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Enemies
import Arkham.Location.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Locations
import Arkham.Scenarios.EdgeOfTheEarth.FatalMirage.Helpers
import Arkham.Story.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Cards
import Arkham.Story.Import.Lifted

newtype DyersClassroom = DyersClassroom StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dyersClassroom :: StoryCard DyersClassroom
dyersClassroom = story DyersClassroom Cards.dyersClassroom

instance RunMessage DyersClassroom where
  runMessage msg s@(DyersClassroom attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      handleMemory
        attrs
        Assets.professorWilliamDyerProfessorOfGeology
        Locations.dyersClassroom
        Enemies.memoryOfARegretfulVoyage
      pure s
    _ -> DyersClassroom <$> liftRunMessage msg attrs
