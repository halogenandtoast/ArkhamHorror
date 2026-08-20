module Arkham.Story.Cards.EdgeOfTheEarth.FatalMirage.ClutteredDormitory (clutteredDormitory) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Enemies
import Arkham.Location.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Locations
import Arkham.Scenarios.EdgeOfTheEarth.FatalMirage.Helpers
import Arkham.Story.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Cards
import Arkham.Story.Import.Lifted

newtype ClutteredDormitory = ClutteredDormitory StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clutteredDormitory :: StoryCard ClutteredDormitory
clutteredDormitory = story ClutteredDormitory Cards.clutteredDormitory

instance RunMessage ClutteredDormitory where
  runMessage msg s@(ClutteredDormitory attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      handleMemory
        attrs
        Assets.danforthBrilliantStudent
        Locations.clutteredDormitory
        Enemies.memoryOfAnUnspeakableEvil
      pure s
    _ -> ClutteredDormitory <$> liftRunMessage msg attrs
