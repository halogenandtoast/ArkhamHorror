module Arkham.Asset.Cards.MacheteSpec (
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
spec = describe "Machete" $ do
  it "gives +1 combat and +1 damage if the attacked enemy is the only enemy engaged with you" $
    gameTest $ \investigator -> do
      updateInvestigator investigator $
        \attrs -> attrs {investigatorCombat = 1}
      putCardIntoPlay investigator Assets.machete
      machete <- selectJust $ assetIs Assets.machete
      enemy <- testEnemy $
        \attrs -> attrs {enemyFight = 2, enemyHealth = Static 3}
      location <- testLocation id
      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ placedLocation location
      pushAndRun $ enemySpawn location enemy
      pushAndRun $ moveTo investigator location
      [doFight] <- field AssetAbilities machete
      pushAndRun $ UseAbility (toId investigator) doFight []
      chooseOnlyOption "choose enemy"
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"

      fieldAssert EnemyDamage (== 2) enemy

  it "does not give additional damage if the attacked enemy is not engaged with you" $ do
    gameTest $ \investigator -> do
      updateInvestigator investigator $
        \attrs -> attrs {investigatorCombat = 1}
      putCardIntoPlay investigator Assets.machete
      machete <- selectJust $ assetIs Assets.machete
      enemy <- testEnemy $
        \attrs -> attrs {enemyFight = 2, enemyHealth = Static 3, enemyExhausted = True}
      location <- testLocation id
      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ placedLocation location
      pushAndRun $ enemySpawn location enemy
      pushAndRun $ moveTo investigator location
      [doFight] <- field AssetAbilities machete
      pushAndRun $ UseAbility (toId investigator) doFight []
      chooseOnlyOption "choose enemy"
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"

      fieldAssert EnemyDamage (== 1) enemy

  it "does not give additional damage if the attacked enemy is not the only enemy engaged with you" $ do
    gameTest $ \investigator -> do
      updateInvestigator investigator $
        \attrs -> attrs {investigatorCombat = 1}
      putCardIntoPlay investigator Assets.machete
      machete <- selectJust $ assetIs Assets.machete
      enemy1 <- testEnemy $
        \attrs -> attrs {enemyFight = 2, enemyHealth = Static 3}
      enemy2 <- testEnemy $
        \attrs -> attrs {enemyFight = 2, enemyHealth = Static 3}
      location <- testLocation id
      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ placedLocation location
      pushAndRun $ enemySpawn location enemy1
      pushAndRun $ enemySpawn location enemy2
      pushAndRun $ moveTo investigator location
      [doFight] <- field AssetAbilities machete
      pushAndRun $ UseAbility (toId investigator) doFight []
      chooseOptionMatching "choose enemy1" $ \case
        FightLabel {enemyId} -> enemyId == toId enemy1
        _ -> False
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"

      fieldAssert EnemyDamage (== 1) enemy1
