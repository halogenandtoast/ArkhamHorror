module Arkham.Story.Cards.AlaskanWilds (alaskanWilds) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype AlaskanWilds = AlaskanWilds StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alaskanWilds :: StoryCard AlaskanWilds
alaskanWilds = story AlaskanWilds Cards.alaskanWilds

instance RunMessage AlaskanWilds where
  runMessage msg s@(AlaskanWilds attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      handleMemory
        attrs
        Assets.eliyahAshevakDogHandler
        Locations.alaskanWilds
        Enemies.memoryOfAHuntGoneAwry
      pure s
    _ -> AlaskanWilds <$> liftRunMessage msg attrs
