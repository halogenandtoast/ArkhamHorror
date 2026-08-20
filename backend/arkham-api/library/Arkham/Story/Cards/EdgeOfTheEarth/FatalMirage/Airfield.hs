module Arkham.Story.Cards.EdgeOfTheEarth.FatalMirage.Airfield (airfield) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Enemies
import Arkham.Location.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Locations
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Cards
import Arkham.Story.Import.Lifted

newtype Airfield = Airfield StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

airfield :: StoryCard Airfield
airfield = story Airfield Cards.airfield

instance RunMessage Airfield where
  runMessage msg s@(Airfield attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      handleMemory
        attrs
        Assets.takadaHirokoAeroplaneMechanic
        Locations.airfield
        Enemies.memoryOfAMissingFather
      pure s
    _ -> Airfield <$> liftRunMessage msg attrs
