module Arkham.Scenarios.TheDunwichLegacy.UndimensionedAndUnseen.Helpers where

import Arkham.Campaigns.TheDunwichLegacy.Helpers
import Arkham.Card (Card)
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Enemy.CardDefs.TheDunwichLegacy.UndimensionedAndUnseen qualified as Cards
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Name
import Arkham.Prelude
import Arkham.UltimatumsAndBoons (hasUltimatum)
import Arkham.UltimatumsAndBoons.Types (Ultimatum (UltimatumOfMultiplication))

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "undimensionedAndUnseen" a

broodTitle :: Text
broodTitle = nameTitle . toName $ Cards.broodOfYogSothoth

getMatchingBroodOfYogSothoth :: HasGame m => EnemyMatcher -> m [EnemyId]
getMatchingBroodOfYogSothoth matcher = select $ EnemyWithTitle broodTitle <> matcher

getBroodOfYogSothoth :: HasGame m => m [EnemyId]
getBroodOfYogSothoth = select $ EnemyWithTitle broodTitle

getSetAsideBroodOfYogSothoth :: HasGame m => m [Card]
getSetAsideBroodOfYogSothoth = getSetAsideCardsMatching $ CardWithTitle broodTitle

{- | Is there a set-aside Brood to spawn? Under the Ultimatum of Multiplication
there never is -- all five start in play -- but the effect still happens, as one
doom on the agenda, so the option must still be offered.
-}
canSpawnSetAsideBroodOfYogSothoth :: HasGame m => m Bool
canSpawnSetAsideBroodOfYogSothoth =
  orM [hasUltimatum UltimatumOfMultiplication, notNull <$> getSetAsideBroodOfYogSothoth]

{- | Spawn a set-aside Brood of Yog-Sothoth at a location, and hand back the
enemy when one was spawned.

The Ultimatum of Multiplication replaces every such spawn with 1 doom on the
current agenda, which is why each of the three spawning cards goes through here
rather than calling 'createEnemyAt' itself.
-}
spawnSetAsideBroodOfYogSothothAt :: ReverseQueue m => LocationId -> m (Maybe EnemyId)
spawnSetAsideBroodOfYogSothothAt lid = do
  multiplication <- hasUltimatum UltimatumOfMultiplication
  if multiplication
    then Nothing <$ placeDoomOnAgendaAndCheckAdvance 1
    else do
      brood <- getSetAsideBroodOfYogSothoth
      for (nonEmpty brood) \xs -> do
        x <- sample xs
        createEnemyAt x lid
