module Arkham.Asset.Cards.FireAxeSpec (
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
spec = describe "Fire Axe" $ do
  it "gives +1 damage if you have no resources" $
    gameTest $ \investigator -> do
      updateInvestigator investigator $
        \attrs -> attrs {investigatorResources = 0, investigatorCombat = 3}
      putCardIntoPlay investigator Assets.fireAxe
      fireAxe <- selectJust $ assetIs Assets.fireAxe
      enemy <- testEnemy $
        \attrs -> attrs {enemyHealth = Static 3, enemyFight = 3}
      location <- testLocation id
      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ enemySpawn location enemy
      pushAndRun $ moveTo investigator location
      [doFight, _] <- field AssetAbilities fireAxe
      pushAndRun $ UseAbility (toId investigator) doFight []
      chooseOnlyOption "Fight enemy"
      chooseOnlyOption "Start skill test"
      chooseOnlyOption "Apply Results"
      fieldAssert EnemyDamage (== 2) enemy

  it "allows you to spend 1 resource to get +2 combat" $
    gameTest $ \investigator -> do
      updateInvestigator investigator $
        \attrs -> attrs {investigatorResources = 2, investigatorCombat = 1}
      putCardIntoPlay investigator Assets.fireAxe
      fireAxe <- selectJust $ assetIs Assets.fireAxe
      enemy <- testEnemy $
        \attrs -> attrs {enemyHealth = Static 3, enemyFight = 3}
      location <- testLocation id
      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ enemySpawn location enemy
      pushAndRun $ moveTo investigator location
      [doFight, _] <- field AssetAbilities fireAxe
      pushAndRun $ UseAbility (toId investigator) doFight []
      chooseOnlyOption "Fight enemy"
      chooseOptionMatching
        "spend resource"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOptionMatching
        "Skip playing fast cards or using reactions and continue"
        ( \case
            Label {} -> True
            _ -> False
        )
      chooseOptionMatching
        "Start Skill Test"
        ( \case
            StartSkillTestButton {} -> True
            _ -> False
        )
      chooseOptionMatching
        "Skip playing fast cards or using reactions and continue"
        ( \case
            Label {} -> True
            _ -> False
        )
      chooseOnlyOption "Apply Results"
      fieldAssert EnemyDamage (== 1) enemy

  it "if you spend your resources before tokens, you still get +1 damage" $
    gameTest $ \investigator -> do
      updateInvestigator investigator $
        \attrs -> attrs {investigatorResources = 1, investigatorCombat = 1}
      putCardIntoPlay investigator Assets.fireAxe
      fireAxe <- selectJust $ assetIs Assets.fireAxe
      enemy <- testEnemy $
        \attrs -> attrs {enemyHealth = Static 3, enemyFight = 3}
      location <- testLocation id

      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ enemySpawn location enemy
      pushAndRun $ moveTo investigator location
      [doFight, _] <- field AssetAbilities fireAxe
      pushAndRun $ UseAbility (toId investigator) doFight []
      chooseOnlyOption "Fight enemy"
      chooseOptionMatching
        "spend resource"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOptionMatching
        "Start Skill Test"
        ( \case
            StartSkillTestButton {} -> True
            _ -> False
        )
      chooseOnlyOption "Apply Results"
      fieldAssert EnemyDamage (== 2) enemy

  it "limit of 3 resources can be spent" $
    gameTest $ \investigator -> do
      updateInvestigator investigator $
        \attrs -> attrs {investigatorResources = 4, investigatorCombat = 1}
      putCardIntoPlay investigator Assets.fireAxe
      fireAxe <- selectJust $ assetIs Assets.fireAxe
      enemy <- testEnemy $
        \attrs -> attrs {enemyHealth = Static 3, enemyFight = 3}
      location <- testLocation id
      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ enemySpawn location enemy
      pushAndRun $ moveTo investigator location
      [doFight, _] <- field AssetAbilities fireAxe
      pushAndRun $ UseAbility (toId investigator) doFight []
      chooseOnlyOption "Fight enemy"
      chooseOptionMatching
        "spend resource"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOptionMatching
        "spend resource"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOptionMatching
        "spend resource"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOnlyOption "Start skill test"
      chooseOnlyOption "Apply Results"
      fieldAssert EnemyDamage (== 1) enemy
