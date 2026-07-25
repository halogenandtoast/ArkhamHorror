module Arkham.Enemy.Cards.ElderDeepOneSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types
import Arkham.Phase
import TestImport.New

-- N.B. testEnemyWithDef zeroes the def's damage, so the thrall has to be given
-- its printed 1 damage back or every attack lands for nothing and the test
-- passes/fails without measuring anything.
thrallAt :: Location -> TestAppT Enemy
thrallAt location = do
  thrall <- testEnemyWithDef Enemies.deepOneThrall id & prop @"healthDamage" 1
  thrall `spawnAt` location
  pure thrall

spec :: Spec
spec = describe "Elder Deep One" do
  it "makes each other Deep One enemy relentless" . gameTest $ \self -> do
    location <- testLocation
    elderLocation <- testLocation
    elderDeepOne <- testEnemyWithDef Enemies.elderDeepOne id
    deepOneThrall <- thrallAt location
    elderDeepOne `spawnAt` elderLocation
    self `moveTo` location

    run $ SetPhase EnemyPhase
    run EnemiesAttack
    chooseTarget deepOneThrall
    applyAllDamage
    chooseTarget deepOneThrall
    applyAllDamage

    self.damage `shouldReturn` 2

  -- control: proves the two attacks above come from the Elder Deep One and not
  -- from the harness resolving one attack twice
  it "attacks once when no Elder Deep One is in play" . gameTest $ \self -> do
    location <- testLocation
    deepOneThrall <- thrallAt location
    self `moveTo` location

    run $ SetPhase EnemyPhase
    run EnemiesAttack
    chooseTarget deepOneThrall
    applyAllDamage

    self.damage `shouldReturn` 1
