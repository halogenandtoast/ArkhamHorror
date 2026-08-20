module Arkham.Story.Cards.ThePathToCarcosa.TheLastKing.SickeningReality_67 (sickeningReality_67) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.ThePathToCarcosa.TheLastKing qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Story.CardDefs.ThePathToCarcosa.TheLastKing qualified as Cards
import Arkham.Story.Import.Lifted

newtype SickeningReality_67 = SickeningReality_67 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sickeningReality_67 :: StoryCard SickeningReality_67
sickeningReality_67 = story SickeningReality_67 Cards.sickeningReality_67

instance RunMessage SickeningReality_67 where
  runMessage msg s@(SickeningReality_67 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      haruko <- selectJust (assetIs Assets.ishimaruHaruko)
      withLocationOf haruko \lid -> do
        selectEach (investigatorAt lid) \iid -> assignHorror iid attrs 1
        moveAllTokens attrs haruko lid #clue
        removeFromGame haruko
        createEnemyAt_ Enemies.ishimaruHaruko lid
      pure s
    _ -> SickeningReality_67 <$> liftRunMessage msg attrs
