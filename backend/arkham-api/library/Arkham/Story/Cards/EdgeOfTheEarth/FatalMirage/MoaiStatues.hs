module Arkham.Story.Cards.EdgeOfTheEarth.FatalMirage.MoaiStatues (moaiStatues) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Enemies
import Arkham.Location.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Locations
import Arkham.Scenarios.EdgeOfTheEarth.FatalMirage.Helpers
import Arkham.Story.CardDefs.EdgeOfTheEarth.FatalMirage qualified as Cards
import Arkham.Story.Import.Lifted

newtype MoaiStatues = MoaiStatues StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moaiStatues :: StoryCard MoaiStatues
moaiStatues = story MoaiStatues Cards.moaiStatues

instance RunMessage MoaiStatues where
  runMessage msg s@(MoaiStatues attrs) = runQueueT $ case msg of
    ResolveThisStory _iid (is attrs -> True) -> do
      handleMemory
        attrs
        Assets.roaldEllsworthIntrepidExplorer
        Locations.moaiStatues
        Enemies.memoryOfAnAlienTranslation
      pure s
    _ -> MoaiStatues <$> liftRunMessage msg attrs
