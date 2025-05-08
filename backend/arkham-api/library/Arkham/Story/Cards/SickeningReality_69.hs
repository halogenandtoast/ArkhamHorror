module Arkham.Story.Cards.SickeningReality_69 (sickeningReality_69) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SickeningReality_69 = SickeningReality_69 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sickeningReality_69 :: StoryCard SickeningReality_69
sickeningReality_69 = story SickeningReality_69 Cards.sickeningReality_69

instance RunMessage SickeningReality_69 where
  runMessage msg s@(SickeningReality_69 attrs) = runQueueT $ case msg of
    ResolveStory _ _ (is attrs -> True) -> do
      ashleigh <- selectJust (assetIs Assets.ashleighClarke)
      withLocationOf ashleigh \lid -> do
        selectEach (investigatorAt lid) \iid -> assignHorror iid attrs 1
        moveAllTokens attrs ashleigh lid #clue
        removeFromGame ashleigh
        createEnemyAt_ Enemies.ashleighClarke lid
      pure s
    _ -> SickeningReality_69 <$> liftRunMessage msg attrs
