module Arkham.Story.Cards.EdgeOfTheEarth.FatalMirage.Infirmary (infirmary) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Enemies
import Arkham.Location.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Locations
import Arkham.Scenarios.EdgeOfTheEarth.FatalMirage.Helpers
import Arkham.Story.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Cards
import Arkham.Story.Import.Lifted

newtype Infirmary = Infirmary StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infirmary :: StoryCard Infirmary
infirmary = story Infirmary Cards.infirmary

instance RunMessage Infirmary where
  runMessage msg s@(Infirmary attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      handleMemory
        attrs
        Assets.drMalaSinhaDaringPhysician
        Locations.infirmaryFatalMirage
        Enemies.memoryOfALostPatient
      pure s
    _ -> Infirmary <$> liftRunMessage msg attrs
