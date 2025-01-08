module Arkham.Story.Cards.DrKenslersOffice (drKenslersOffice) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype DrKenslersOffice = DrKenslersOffice StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drKenslersOffice :: StoryCard DrKenslersOffice
drKenslersOffice = story DrKenslersOffice Cards.drKenslersOffice

instance RunMessage DrKenslersOffice where
  runMessage msg s@(DrKenslersOffice attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      handleMemory
        attrs
        Assets.drAmyKenslerProfessorOfBiology
        Locations.drKenslersOffice
        Enemies.memoryOfAnUnrequitedLove
      pure s
    _ -> DrKenslersOffice <$> liftRunMessage msg attrs
