module Arkham.Asset.Cards.FortyFiveAutomaticSpec (
  spec,
) where

import TestImport hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Types (EnemyAttrs (..), Field (..))
import Arkham.Investigator.Types (InvestigatorAttrs (..))
import Arkham.Matcher (assetIs)
import Arkham.Projection

spec :: Spec
spec = describe ".45 Automatic" $ do
  it "gives +1 combat and +1 damage" $
    gameTest $ \investigator -> do
      updateInvestigator investigator $
        \attrs -> attrs {investigatorCombat = 1}
      putCardIntoPlay investigator Assets.fortyFiveAutomatic
      fortyFiveAutomatic <- selectJust $ assetIs Assets.fortyFiveAutomatic
      enemy <- testEnemyWith $
        \attrs -> attrs {enemyFight = 2, enemyHealth = Static 3}
      location <- testLocationWith id
      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ placedLocation location
      pushAndRun $ spawnAt enemy location
      pushAndRun $ moveTo investigator location
      [doFight] <- field AssetAbilities fortyFiveAutomatic
      pushAndRun $ UseAbility (toId investigator) doFight []
      chooseOnlyOption "choose enemy"
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"

      fieldAssert EnemyDamage (== 2) enemy
