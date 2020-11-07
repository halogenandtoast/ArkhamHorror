module Arkham.Types.Investigator.Cards.ZoeySamarasSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as Enemy

spec :: Spec
spec = do
  describe "Zoey Samaras" $ do
    it "elder sign token gives +1" $ do
      let zoeySamaras = lookupInvestigator "02001"
      game <- runGameTest zoeySamaras [] id
      token <- withGame
        game
        (getTokenValue zoeySamaras (getId () zoeySamaras) ElderSign)
      tokenValue token `shouldBe` Just 1
    it "elder sign token gives +1 and does +1 damage for attacks" $ do
      let zoeySamaras = lookupInvestigator "02001" -- combat is 4
      enemy <- testEnemy ((Enemy.health .~ Static 3) . (Enemy.fight .~ 5))
      scenario' <- testScenario "00000" id
      location <- testLocation "00000" id
      game <-
        runGameTest
          zoeySamaras
          [ SetTokens [ElderSign]
          , enemySpawn location enemy
          , moveTo zoeySamaras location
          , fightEnemy zoeySamaras enemy
          ]
          ((enemies %~ insertEntity enemy)
          . (locations %~ insertEntity location)
          . (scenario ?~ scenario')
          )
        >>= runGameTestOptionMatching
              "skip ability"
              (\case
                Continue{} -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOption "apply results"
      updated game enemy `shouldSatisfy` hasDamage (2, 0)
    it "allows you to gain a resource each time you are engaged by an enemy"
      $ do
          let zoeySamaras = lookupInvestigator "02001"
          location <- testLocation "00000" id
          enemy1 <- testEnemy id
          enemy2 <- testEnemy id
          game <-
            runGameTest
              zoeySamaras
              [ enemySpawn location enemy1
              , moveTo zoeySamaras location
              , enemySpawn location enemy2
              ]
              ((locations %~ insertEntity location)
              . (enemies %~ insertEntity enemy1)
              . (enemies %~ insertEntity enemy2)
              )
            >>= runGameTestOptionMatching
                  "use ability"
                  (\case
                    Run{} -> True
                    _ -> False
                  )
            >>= runGameTestOptionMatching
                  "use ability again"
                  (\case
                    Run{} -> True
                    _ -> False
                  )
          updatedResourceCount game zoeySamaras `shouldBe` 2
