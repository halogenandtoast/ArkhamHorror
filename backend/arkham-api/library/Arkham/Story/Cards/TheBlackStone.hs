module Arkham.Story.Cards.TheBlackStone (theBlackStone) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TheBlackStone = TheBlackStone StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackStone :: StoryCard TheBlackStone
theBlackStone = story TheBlackStone Cards.theBlackStone

instance RunMessage TheBlackStone where
  runMessage msg s@(TheBlackStone attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      handleMemory
        attrs
        Assets.averyClaypoolAntarcticGuide
        Locations.theBlackStone
        Enemies.memoryOfATerribleDiscovery
      pure s
    _ -> TheBlackStone <$> liftRunMessage msg attrs
