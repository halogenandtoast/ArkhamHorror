module Arkham.Event.Cards.SneakAttackSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Sneak Attack" $ do
  it "deals 2 damage to an exhausted enemy at your location" . gameTest $ \self -> do
    location <- testLocation
    enemy1 <- testEnemy & prop @"health" 3 & exhausted
    enemy2 <- testEnemy & prop @"health" 3 & exhausted
    enemy3 <- testEnemy & prop @"health" 3
    self `moveTo` location
    enemy1 `spawnAt` location
    enemy2 `spawnAt` location
    enemy3 `spawnAt` location
    self `playEvent` Events.sneakAttack
    assertTarget enemy2
    assertNotTarget enemy3
    chooseTarget enemy1
    enemy1.damage `shouldReturn` 2
    enemy2.damage `shouldReturn` 0
    enemy3.damage `shouldReturn` 0
