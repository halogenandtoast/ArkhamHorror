module Arkham.Story.Cards.ClutteredDormitory (clutteredDormitory) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ClutteredDormitory = ClutteredDormitory StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clutteredDormitory :: StoryCard ClutteredDormitory
clutteredDormitory = story ClutteredDormitory Cards.clutteredDormitory

instance RunMessage ClutteredDormitory where
  runMessage msg s@(ClutteredDormitory attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      handleMemory
        attrs
        Assets.danforthBrilliantStudent
        Locations.clutteredDormitory
        Enemies.memoryOfAnUnspeakableEvil
      pure s
    _ -> ClutteredDormitory <$> liftRunMessage msg attrs
