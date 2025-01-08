module Arkham.Story.Cards.Airfield (airfield) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype Airfield = Airfield StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

airfield :: StoryCard Airfield
airfield = story Airfield Cards.airfield

instance RunMessage Airfield where
  runMessage msg s@(Airfield attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      handleMemory
        attrs
        Assets.takadaHirokoAeroplaneMechanic
        Locations.airfield
        Enemies.memoryOfAMissingFather
      pure s
    _ -> Airfield <$> liftRunMessage msg attrs
