module Arkham.Story.Cards.EdgeOfTheEarth.FatalMirage.DrKenslersOffice (drKenslersOffice) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Enemies
import Arkham.Location.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Locations
import Arkham.Scenarios.EdgeOfTheEarth.FatalMirage.Helpers
import Arkham.Story.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Cards
import Arkham.Story.Import.Lifted

newtype DrKenslersOffice = DrKenslersOffice StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drKenslersOffice :: StoryCard DrKenslersOffice
drKenslersOffice = story DrKenslersOffice Cards.drKenslersOffice

instance RunMessage DrKenslersOffice where
  runMessage msg s@(DrKenslersOffice attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      handleMemory
        attrs
        Assets.drAmyKenslerProfessorOfBiology
        Locations.drKenslersOffice
        Enemies.memoryOfAnUnrequitedLove
      pure s
    _ -> DrKenslersOffice <$> liftRunMessage msg attrs
