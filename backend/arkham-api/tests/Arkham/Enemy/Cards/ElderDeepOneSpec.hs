module Arkham.Enemy.Cards.ElderDeepOneSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Phase
import TestImport.New

spec :: Spec
spec = describe "Elder Deep One" do
  it "makes each other Deep One enemy relentless" . gameTest $ \self -> do
    location <- testLocation
    elderLocation <- testLocation
    elderDeepOne <- testEnemyWithDef Enemies.elderDeepOne id
    deepOneThrall <- testEnemyWithDef Enemies.deepOneThrall id
    elderDeepOne `spawnAt` elderLocation
    deepOneThrall `spawnAt` location
    self `moveTo` location

    run $ SetPhase EnemyPhase
    run EnemiesAttack
    chooseTarget deepOneThrall
    applyAllDamage
    applyAllDamage

    self.damage `shouldReturn` 2
