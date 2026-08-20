module Arkham.Story.Cards.EdgeOfTheEarth.FatalMirage.OttomanFront (ottomanFront) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Enemies
import Arkham.Location.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Locations
import Arkham.Scenarios.EdgeOfTheEarth.FatalMirage.Helpers
import Arkham.Story.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Cards
import Arkham.Story.Import.Lifted

newtype OttomanFront = OttomanFront StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ottomanFront :: StoryCard OttomanFront
ottomanFront = story OttomanFront Cards.ottomanFront

instance RunMessage OttomanFront where
  runMessage msg s@(OttomanFront attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      handleMemory
        attrs
        Assets.jamesCookieFredericksDubiousChoice
        Locations.ottomanFront
        Enemies.memoryOfARavagedCountry
      pure s
    _ -> OttomanFront <$> liftRunMessage msg attrs
