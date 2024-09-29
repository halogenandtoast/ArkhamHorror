module Arkham.Investigator.Cards.ZoeySamarasSpec (spec) where

import TestImport.New hiding (EnemyDamage)

import Arkham.Classes.HasChaosTokenValue
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Game ()
import Arkham.Investigator.Cards qualified as Investigators

spec :: Spec
spec = do
  describe "Zoey Samaras" $ do
    it "elder sign token gives +1" $ gameTestWith Investigators.zoeySamaras $ \zoeySamaras -> do
      token <- getChaosTokenValue (toId zoeySamaras) ElderSign (toId zoeySamaras)
      chaosTokenValue token `shouldReturn` Just 1

    it "elder sign token gives +1 and does +1 damage for attacks" . gameTestWith Investigators.zoeySamaras $ \zoeySamaras -> do
      enemy <- testEnemyWith ((Enemy.healthL ?~ Static 3) . (Enemy.fightL ?~ 5))
      location <- testLocationWith id
      setChaosTokens [ElderSign]
      enemy `spawnAt` location
      zoeySamaras `moveTo` location
      skip
      _ <- fightEnemy zoeySamaras enemy
      click "start skill test"
      click "apply results"
      enemy.damage `shouldReturn` 2

    it "allows you to gain a resource each time you are engaged by an enemy"
      $ gameTestWith Investigators.zoeySamaras
      $ \zoeySamaras -> do
        location <- testLocationWith id
        enemy1 <- testEnemyWith id
        enemy2 <- testEnemyWith id
        enemy1 `spawnAt` location
        zoeySamaras `moveTo` location
        chooseOptionMatching "use ability" \case
          AbilityLabel {} -> True
          _ -> False
        enemy2 `spawnAt` location
        chooseOptionMatching "use ability again" \case
          AbilityLabel {} -> True
          _ -> False
        zoeySamaras.resources `shouldReturn` 2
