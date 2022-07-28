module Arkham.Investigator.Cards.ZoeySamarasSpec
  ( spec
  ) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Enemy.Types qualified as Enemy
import Arkham.Investigator.Types (Field (..))
import Arkham.Enemy.Types (Field (..))
import Arkham.Game ()
import Arkham.Classes.HasTokenValue

spec :: Spec
spec = do
  describe "Zoey Samaras" $ do
    it "elder sign token gives +1" $ do
      let zoeySamaras = lookupInvestigator "02001"
      gameTest zoeySamaras [] id $ do
        runMessages
        token <- getTokenValue (toId zoeySamaras) ElderSign (toId zoeySamaras)
        tokenValue token `shouldBe` Just 1

    it "elder sign token gives +1 and does +1 damage for attacks" $ do
      let zoeySamaras = lookupInvestigator "02001" -- combat is 4
      enemy <- testEnemy ((Enemy.healthL .~ Static 3) . (Enemy.fightL .~ 5))
      location <- testLocation id
      gameTest
          zoeySamaras
          [ SetTokens [ElderSign]
          , enemySpawn location enemy
          , moveTo zoeySamaras location
          , fightEnemy zoeySamaras enemy
          ]
          ((entitiesL . enemiesL %~ insertEntity enemy)
          . (entitiesL . locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOptionMatching
              "skip ability"
              (\case
                Continue{} -> True
                _ -> False
              )
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            fieldAssert EnemyDamage (== 2) enemy

    it "allows you to gain a resource each time you are engaged by an enemy"
      $ do
          let zoeySamaras = lookupInvestigator "02001"
          location <- testLocation id
          enemy1 <- testEnemy id
          enemy2 <- testEnemy id
          gameTest
              zoeySamaras
              [ enemySpawn location enemy1
              , moveTo zoeySamaras location
              , enemySpawn location enemy2
              ]
              ((entitiesL . locationsL %~ insertEntity location)
              . (entitiesL . enemiesL %~ insertEntity enemy1)
              . (entitiesL . enemiesL %~ insertEntity enemy2)
              )
            $ do
                runMessages
                chooseOptionMatching
                  "use ability"
                  (\case
                    Run{} -> True
                    _ -> False
                  )
                chooseOptionMatching
                  "use ability again"
                  (\case
                    Run{} -> True
                    _ -> False
                  )
                fieldAssert InvestigatorResources (== 2) zoeySamaras
