module Arkham.Types.Asset.Cards.FireAxeSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Enemy.Attrs (Attrs(..))
import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = describe "Fire Axe" $ do
  it "gives +1 damage if you have no resources" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorResources = 0, investigatorCombat = 3 }
    fireAxe <- buildAsset "02032"
    enemy <- testEnemy
      $ \attrs -> attrs { enemyHealth = Static 3, enemyFight = 3 }
    location <- testLocation "00000" id
    scenario' <- testScenario "00000" id
    game <- runGameTest
      investigator
      [ SetTokens [Zero]
      , playAsset investigator fireAxe
      , enemySpawn location enemy
      , moveTo investigator location
      ]
      ((scenario ?~ scenario')
      . (enemies %~ insertEntity enemy)
      . (assets %~ insertEntity fireAxe)
      . (locations %~ insertEntity location)
      )
    [fightAction] <- getActionsOf game investigator NonFast fireAxe
    game' <-
      runGameTestMessages game [fightAction]
      >>= runGameTestOnlyOption "Fight enemy"
      >>= runGameTestOnlyOption "Start skill test"
      >>= runGameTestOnlyOption "Apply Results"
    updated game' enemy `shouldSatisfy` hasDamage (2, 0)

  it "allows you to spend 1 resource to get +2 combat" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorResources = 2, investigatorCombat = 1 }
    fireAxe <- buildAsset "02032"
    enemy <- testEnemy
      $ \attrs -> attrs { enemyHealth = Static 3, enemyFight = 3 }
    location <- testLocation "00000" id
    scenario' <- testScenario "00000" id
    game <- runGameTest
      investigator
      [ SetTokens [Zero]
      , playAsset investigator fireAxe
      , enemySpawn location enemy
      , moveTo investigator location
      ]
      ((scenario ?~ scenario')
      . (enemies %~ insertEntity enemy)
      . (assets %~ insertEntity fireAxe)
      . (locations %~ insertEntity location)
      )
    [fightAction] <- getActionsOf game investigator NonFast fireAxe
    game' <-
      runGameTestMessages game [fightAction]
      >>= runGameTestOnlyOption "Fight enemy"
      >>= runGameTestOptionMatching
            "spend resource"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOptionMatching
            "Start Skill Test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOption "Apply Results"
    updated game' enemy `shouldSatisfy` hasDamage (1, 0)

  it "if you spend your resources before tokens, you stil get +1 damage" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorResources = 1, investigatorCombat = 1 }
    fireAxe <- buildAsset "02032"
    enemy <- testEnemy
      $ \attrs -> attrs { enemyHealth = Static 3, enemyFight = 3 }
    location <- testLocation "00000" id
    scenario' <- testScenario "00000" id
    game <- runGameTest
      investigator
      [ SetTokens [Zero]
      , playAsset investigator fireAxe
      , enemySpawn location enemy
      , moveTo investigator location
      ]
      ((scenario ?~ scenario')
      . (enemies %~ insertEntity enemy)
      . (assets %~ insertEntity fireAxe)
      . (locations %~ insertEntity location)
      )
    [fightAction] <- getActionsOf game investigator NonFast fireAxe
    game' <-
      runGameTestMessages game [fightAction]
      >>= runGameTestOnlyOption "Fight enemy"
      >>= runGameTestOptionMatching
            "spend resource"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOptionMatching
            "Start Skill Test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOption "Apply Results"
    updated game' enemy `shouldSatisfy` hasDamage (2, 0)

  it "limit of 3 resources can be spent" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorResources = 4, investigatorCombat = 1 }
    fireAxe <- buildAsset "02032"
    enemy <- testEnemy
      $ \attrs -> attrs { enemyHealth = Static 3, enemyFight = 3 }
    location <- testLocation "00000" id
    scenario' <- testScenario "00000" id
    game <- runGameTest
      investigator
      [ SetTokens [Zero]
      , playAsset investigator fireAxe
      , enemySpawn location enemy
      , moveTo investigator location
      ]
      ((scenario ?~ scenario')
      . (enemies %~ insertEntity enemy)
      . (assets %~ insertEntity fireAxe)
      . (locations %~ insertEntity location)
      )
    [fightAction] <- getActionsOf game investigator NonFast fireAxe
    game' <-
      runGameTestMessages game [fightAction]
      >>= runGameTestOnlyOption "Fight enemy"
      >>= runGameTestOptionMatching
            "spend resource"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOptionMatching
            "spend resource"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOptionMatching
            "spend resource"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOption "Start skill test"
      >>= runGameTestOnlyOption "Apply Results"
    updated game' enemy `shouldSatisfy` hasDamage (1, 0)
