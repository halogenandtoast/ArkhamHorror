module Arkham.Story.Cards.EdgeOfTheEarth.FatalMirage.AlaskanWilds (alaskanWilds) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Enemies
import Arkham.Location.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Locations
import Arkham.Scenarios.EdgeOfTheEarth.FatalMirage.Helpers
import Arkham.Story.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Cards
import Arkham.Story.Import.Lifted

newtype AlaskanWilds = AlaskanWilds StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alaskanWilds :: StoryCard AlaskanWilds
alaskanWilds = story AlaskanWilds Cards.alaskanWilds

instance RunMessage AlaskanWilds where
  runMessage msg s@(AlaskanWilds attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      handleMemory
        attrs
        Assets.eliyahAshevakDogHandler
        Locations.alaskanWilds
        Enemies.memoryOfAHuntGoneAwry
      pure s
    _ -> AlaskanWilds <$> liftRunMessage msg attrs
