module Arkham.Asset.Assets.FireAxeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Calculation
import Arkham.Enemy.Types (EnemyAttrs (..), Field (..))
import Arkham.Matcher (assetIs)
import Arkham.Projection
import Arkham.Token
import TestImport.New hiding (EnemyDamage)

spec :: Spec
spec = describe "Fire Axe" $ do
  it "gives +1 damage if you have no resources"
    $ gameTest
    $ \investigator -> do
      updateInvestigator investigator
        $ \attrs -> attrs {investigatorTokens = setTokens Resource 0 mempty, investigatorCombat = 3}
      putCardIntoPlay investigator Assets.fireAxe
      fireAxe <- selectJust $ assetIs Assets.fireAxe
      enemy <- testEnemyWith
        $ \attrs -> attrs {enemyHealth = Just (Fixed 3), enemyFight = Just (Fixed 3)}
      location <- testLocationWith id
      setChaosTokens [Zero]
      spawnAt enemy location
      moveTo investigator location
      [doFight, _] <- field AssetAbilities fireAxe
      pushAndRun $ UseAbility (toId investigator) doFight []
      chooseOnlyOption "Fight enemy"
      chooseOnlyOption "Start skill test"
      chooseOnlyOption "Apply Results"
      fieldAssert EnemyDamage (== 2) enemy

  it "allows you to spend 1 resource to get +2 combat"
    $ gameTest
    $ \investigator -> do
      updateInvestigator investigator
        $ \attrs -> attrs {investigatorTokens = setTokens Resource 2 mempty, investigatorCombat = 1}
      putCardIntoPlay investigator Assets.fireAxe
      fireAxe <- selectJust $ assetIs Assets.fireAxe
      enemy <- testEnemyWith
        $ \attrs -> attrs {enemyHealth = Just (Fixed 3), enemyFight = Just (Fixed 3)}
      location <- testLocationWith id
      setChaosTokens [Zero]
      spawnAt enemy location
      moveTo investigator location
      [doFight, _] <- field AssetAbilities fireAxe
      pushAndRun $ UseAbility (toId investigator) doFight []
      chooseOnlyOption "Fight enemy"
      chooseOptionMatching
        "spend resource"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      skip
      chooseOptionMatching
        "Start Skill Test"
        ( \case
            StartSkillTestButton {} -> True
            _ -> False
        )
      skip
      chooseOnlyOption "Apply Results"
      fieldAssert EnemyDamage (== 1) enemy

  it "if you spend your resources before tokens, you still get +1 damage"
    $ gameTest
    $ \investigator -> do
      updateInvestigator investigator
        $ \attrs -> attrs {investigatorTokens = setTokens Resource 1 mempty, investigatorCombat = 1}
      putCardIntoPlay investigator Assets.fireAxe
      fireAxe <- selectJust $ assetIs Assets.fireAxe
      enemy <- testEnemyWith
        $ \attrs -> attrs {enemyHealth = Just (Fixed 3), enemyFight = Just (Fixed 3)}
      location <- testLocationWith id

      setChaosTokens [Zero]
      spawnAt enemy location
      moveTo investigator location
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

  it "limit of 3 resources can be spent"
    $ gameTest
    $ \investigator -> do
      updateInvestigator investigator
        $ \attrs -> attrs {investigatorTokens = setTokens Resource 4 mempty, investigatorCombat = 1}
      putCardIntoPlay investigator Assets.fireAxe
      fireAxe <- selectJust $ assetIs Assets.fireAxe
      enemy <- testEnemyWith
        $ \attrs -> attrs {enemyHealth = Just (Fixed 3), enemyFight = Just (Fixed 3)}
      location <- testLocationWith id
      setChaosTokens [Zero]
      spawnAt enemy location
      moveTo investigator location
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
