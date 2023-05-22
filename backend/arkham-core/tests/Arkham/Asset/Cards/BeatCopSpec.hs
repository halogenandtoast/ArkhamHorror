module Arkham.Asset.Cards.BeatCopSpec (
  spec,
) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Types (EnemyAttrs (..), Field (..))
import Arkham.Helpers.Investigator (modifiedStatsOf)
import Arkham.Investigator.Types (InvestigatorAttrs (..))
import Arkham.Matcher
import Arkham.Projection

spec :: Spec
spec = describe "Beat Cop" $ do
  it "gives you +1 combat" $
    gameTest $ \investigator -> do
      updateInvestigator investigator $
        \attrs -> attrs {investigatorCombat = 1}
      putCardIntoPlay investigator Assets.beatCop
      stats <-
        modifiedStatsOf
          Nothing
          (toId investigator)
      combat stats `shouldBe` 2

  it "can be discarded to do 1 damage to an enemy at your location" $
    gameTest $ \investigator -> do
      updateInvestigator investigator $
        \attrs -> attrs {investigatorCombat = 1}
      putCardIntoPlay investigator Assets.beatCop
      enemy <- testEnemy $
        \attrs -> attrs {enemyHealth = Static 2}
      location <- testLocation id
      pushAndRun $ SetTokens [Zero]
      pushAndRun $ placedLocation location
      pushAndRun $ enemySpawn location enemy
      pushAndRun $ moveTo investigator location
      beatCop <- selectJust $ assetIs Assets.beatCop
      [discardAbility] <- field AssetAbilities beatCop
      pushAndRun $ UseAbility (toId investigator) discardAbility []
      fieldAssert EnemyDamage (== 1) enemy
