module Arkham.Story.Cards.MoaiStatues (moaiStatues) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MoaiStatues = MoaiStatues StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moaiStatues :: StoryCard MoaiStatues
moaiStatues = story MoaiStatues Cards.moaiStatues

instance RunMessage MoaiStatues where
  runMessage msg s@(MoaiStatues attrs) = runQueueT $ case msg of
    ResolveStory _iid ResolveIt story' | story' == toId attrs -> do
      handleMemory
        attrs
        Assets.roaldEllsworthIntrepidExplorer
        Locations.moaiStatues
        Enemies.memoryOfAnAlienTranslation
      pure s
    _ -> MoaiStatues <$> liftRunMessage msg attrs
