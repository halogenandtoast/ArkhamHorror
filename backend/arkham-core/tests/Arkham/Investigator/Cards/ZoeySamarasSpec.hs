module Arkham.Investigator.Cards.ZoeySamarasSpec (
  spec,
) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Classes.HasChaosTokenValue
import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Game ()
import Arkham.Investigator.Cards qualified as Investigators

spec :: Spec
spec = do
  describe "Zoey Samaras" $ do
    it "elder sign token gives +1" $ gameTestWith Investigators.zoeySamaras $ \zoeySamaras -> do
      token <- getChaosTokenValue (toId zoeySamaras) ElderSign (toId zoeySamaras)
      chaosTokenValue token `shouldBe` Just 1

    it "elder sign token gives +1 and does +1 damage for attacks" $ gameTestWith Investigators.zoeySamaras $ \zoeySamaras -> do
      enemy <- testEnemyWith ((Enemy.healthL .~ Static 3) . (Enemy.fightL .~ 5))
      location <- testLocationWith id
      pushAndRunAll
        [ SetChaosTokens [ElderSign]
        , spawnAt enemy location
        , moveTo zoeySamaras location
        , fightEnemy zoeySamaras enemy
        ]
      chooseOptionMatching
        "skip ability"
        ( \case
            Label {} -> True
            _ -> False
        )
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      fieldAssert EnemyDamage (== 2) enemy

    it "allows you to gain a resource each time you are engaged by an enemy"
      $ gameTestWith Investigators.zoeySamaras
      $ \zoeySamaras -> do
        location <- testLocationWith id
        enemy1 <- testEnemyWith id
        enemy2 <- testEnemyWith id
        pushAndRunAll
          [ spawnAt enemy1 location
          , moveTo zoeySamaras location
          , spawnAt enemy2 location
          ]
        chooseOptionMatching
          "use ability"
          ( \case
              AbilityLabel {} -> True
              _ -> False
          )
        chooseOptionMatching
          "use ability again"
          ( \case
              AbilityLabel {} -> True
              _ -> False
          )
        fieldAssert InvestigatorResources (== 2) zoeySamaras
