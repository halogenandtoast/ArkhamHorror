module Arkham.Story.Cards.ThePathToCarcosa.TheLastKing.SickeningReality_65 (sickeningReality_65) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.ThePathToCarcosa.TheLastKing qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Story.CardDefs.ThePathToCarcosa.TheLastKing qualified as Cards
import Arkham.Story.Import.Lifted

newtype SickeningReality_65 = SickeningReality_65 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sickeningReality_65 :: StoryCard SickeningReality_65
sickeningReality_65 = story SickeningReality_65 Cards.sickeningReality_65

instance RunMessage SickeningReality_65 where
  runMessage msg s@(SickeningReality_65 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      constance <- selectJust (assetIs Assets.constanceDumaine)
      withLocationOf constance \lid -> do
        selectEach (investigatorAt lid) \iid -> assignHorror iid attrs 1
        moveAllTokens attrs constance lid #clue
        removeFromGame constance
        createEnemyAt_ Enemies.constanceDumaine lid
      pure s
    _ -> SickeningReality_65 <$> liftRunMessage msg attrs
