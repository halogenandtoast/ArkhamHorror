module Arkham.Story.Cards.OttomanFront (ottomanFront) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype OttomanFront = OttomanFront StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ottomanFront :: StoryCard OttomanFront
ottomanFront = story OttomanFront Cards.ottomanFront

instance RunMessage OttomanFront where
  runMessage msg s@(OttomanFront attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      handleMemory
        attrs
        Assets.jamesCookieFredericksDubiousChoice
        Locations.ottomanFront
        Enemies.memoryOfARavagedCountry
      pure s
    _ -> OttomanFront <$> liftRunMessage msg attrs
