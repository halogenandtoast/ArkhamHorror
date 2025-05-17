module Arkham.Story.Cards.SickeningReality_66 (sickeningReality_66) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SickeningReality_66 = SickeningReality_66 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sickeningReality_66 :: StoryCard SickeningReality_66
sickeningReality_66 = story SickeningReality_66 Cards.sickeningReality_66

instance RunMessage SickeningReality_66 where
  runMessage msg s@(SickeningReality_66 attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      jordan <- selectJust (assetIs Assets.jordanPerry)
      withLocationOf jordan \lid -> do
        selectEach (investigatorAt lid) \iid -> assignHorror iid attrs 1
        moveAllTokens attrs jordan lid #clue
        removeFromGame jordan
        createEnemyAt_ Enemies.jordanPerry lid
      pure s
    _ -> SickeningReality_66 <$> liftRunMessage msg attrs
