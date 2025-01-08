module Arkham.Story.Cards.Infirmary (infirmary) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype Infirmary = Infirmary StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infirmary :: StoryCard Infirmary
infirmary = story Infirmary Cards.infirmary

instance RunMessage Infirmary where
  runMessage msg s@(Infirmary attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      handleMemory
        attrs
        Assets.drMalaSinhaDaringPhysician
        Locations.infirmaryFatalMirage
        Enemies.memoryOfALostPatient
      pure s
    _ -> Infirmary <$> liftRunMessage msg attrs
