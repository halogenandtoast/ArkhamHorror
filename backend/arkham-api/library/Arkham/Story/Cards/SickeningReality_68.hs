module Arkham.Story.Cards.SickeningReality_68 (sickeningReality_68) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SickeningReality_68 = SickeningReality_68 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sickeningReality_68 :: StoryCard SickeningReality_68
sickeningReality_68 = story SickeningReality_68 Cards.sickeningReality_68

instance RunMessage SickeningReality_68 where
  runMessage msg s@(SickeningReality_68 attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      sebastien <- selectJust (assetIs Assets.sebastienMoreau)
      withLocationOf sebastien \lid -> do
        selectEach (investigatorAt lid) \iid -> assignHorror iid attrs 1
        moveAllTokens attrs sebastien lid #clue
        removeFromGame sebastien
        createEnemyAt_ Enemies.sebastienMoreau lid
      pure s
    _ -> SickeningReality_68 <$> liftRunMessage msg attrs
