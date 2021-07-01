module Arkham.Types.Asset.Cards.FireAxeSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Enemy.Attrs (EnemyAttrs(..))
import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Fire Axe" $ do
  it "gives +1 damage if you have no resources" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorResources = 0, investigatorCombat = 3 }
    fireAxe <- buildAsset "02032"
    enemy <- testEnemy
      $ \attrs -> attrs { enemyHealth = Static 3, enemyFight = 3 }
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator fireAxe
        , enemySpawn location enemy
        , moveTo investigator location
        ]
        ((enemiesL %~ insertEntity enemy)
        . (assetsL %~ insertEntity fireAxe)
        . (locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          [doFight] <- getActionsOf investigator NonFast fireAxe
          unshiftMessage doFight
          runMessages
          chooseOnlyOption "Fight enemy"
          chooseOnlyOption "Start skill test"
          chooseOnlyOption "Apply Results"
          updated enemy `shouldSatisfyM` hasDamage (2, 0)

  it "allows you to spend 1 resource to get +2 combat" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorResources = 2, investigatorCombat = 1 }
    fireAxe <- buildAsset "02032"
    enemy <- testEnemy
      $ \attrs -> attrs { enemyHealth = Static 3, enemyFight = 3 }
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator fireAxe
        , enemySpawn location enemy
        , moveTo investigator location
        ]
        ((enemiesL %~ insertEntity enemy)
        . (assetsL %~ insertEntity fireAxe)
        . (locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          [doFight] <- getActionsOf investigator NonFast fireAxe
          unshiftMessage doFight
          runMessages
          chooseOnlyOption "Fight enemy"
          chooseOptionMatching
            "spend resource"
            (\case
              Run{} -> True
              _ -> False
            )
          chooseOptionMatching
            "Start Skill Test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
          chooseOnlyOption "Apply Results"
          updated enemy `shouldSatisfyM` hasDamage (1, 0)

  it "if you spend your resources before tokens, you stil get +1 damage" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorResources = 1, investigatorCombat = 1 }
    fireAxe <- buildAsset "02032"
    enemy <- testEnemy
      $ \attrs -> attrs { enemyHealth = Static 3, enemyFight = 3 }
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator fireAxe
        , enemySpawn location enemy
        , moveTo investigator location
        ]
        ((enemiesL %~ insertEntity enemy)
        . (assetsL %~ insertEntity fireAxe)
        . (locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          [doFight] <- getActionsOf investigator NonFast fireAxe
          unshiftMessage doFight
          runMessages
          chooseOnlyOption "Fight enemy"
          chooseOptionMatching
            "spend resource"
            (\case
              Run{} -> True
              _ -> False
            )
          chooseOptionMatching
            "Start Skill Test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
          chooseOnlyOption "Apply Results"
          updated enemy `shouldSatisfyM` hasDamage (2, 0)

  it "limit of 3 resources can be spent" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorResources = 4, investigatorCombat = 1 }
    fireAxe <- buildAsset "02032"
    enemy <- testEnemy
      $ \attrs -> attrs { enemyHealth = Static 3, enemyFight = 3 }
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator fireAxe
        , enemySpawn location enemy
        , moveTo investigator location
        ]
        ((enemiesL %~ insertEntity enemy)
        . (assetsL %~ insertEntity fireAxe)
        . (locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          [doFight] <- getActionsOf investigator NonFast fireAxe
          unshiftMessage doFight
          runMessages
          chooseOnlyOption "Fight enemy"
          chooseOptionMatching
            "spend resource"
            (\case
              Run{} -> True
              _ -> False
            )
          chooseOptionMatching
            "spend resource"
            (\case
              Run{} -> True
              _ -> False
            )
          chooseOptionMatching
            "spend resource"
            (\case
              Run{} -> True
              _ -> False
            )
          chooseOnlyOption "Start skill test"
          chooseOnlyOption "Apply Results"
          updated enemy `shouldSatisfyM` hasDamage (1, 0)
