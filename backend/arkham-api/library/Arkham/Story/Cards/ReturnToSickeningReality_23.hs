module Arkham.Story.Cards.ReturnToSickeningReality_23 (returnToSickeningReality_23) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ReturnToSickeningReality_23 = ReturnToSickeningReality_23 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToSickeningReality_23 :: StoryCard ReturnToSickeningReality_23
returnToSickeningReality_23 = story ReturnToSickeningReality_23 Cards.returnToSickeningReality_23

instance RunMessage ReturnToSickeningReality_23 where
  runMessage msg s@(ReturnToSickeningReality_23 attrs) = runQueueT $ case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      dianne <- selectJust (assetIs Assets.dianneDevineHidingAnOathUnspoken)
      withLocationOf dianne \lid -> do
        selectEach (investigatorAt lid) \iid -> assignHorror iid attrs 1
        moveAllTokens attrs dianne lid #clue
        removeFromGame dianne
        createEnemyAt_ Enemies.dianneDevineKnowsWhatYoureUpTo lid
      pure s
    _ -> ReturnToSickeningReality_23 <$> liftRunMessage msg attrs
