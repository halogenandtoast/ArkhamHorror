module Arkham.Event.Events.ToeToToeSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Toe to Toe" do
  -- As an additional cost the chosen enemy makes an attack against you; a forced
  -- attack happens even while the enemy is exhausted.
  it "forces the chosen enemy to attack you even while it is exhausted" . gameTest $ \self -> do
    withProp @"combat" 4 self
    enemy <- testEnemy & prop @"fight" 2 & prop @"health" 3 & prop @"healthDamage" 1 & exhausted
    location <- testLocation
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy `spawnAt` location
    self `moveTo` location
    self `playEvent` Events.toeToToe
    chooseTarget enemy
    startSkillTest
    applyResults
    self.damage `shouldReturn` 1
    enemy.damage `shouldReturn` 2
