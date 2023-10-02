module Arkham.Event.Cards.DynamiteBlastSpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

spec :: Spec
spec = describe "Dynamite Blast" $ do
  it "does 3 damage to each enemy and investigator at your location" $ gameTest $ \self -> do
    investigator2 <- addInvestigator Investigators.rolandBanks
    enemy1 <- testEnemy & prop @"health" 4
    enemy2 <- testEnemy & prop @"health" 4
    location <- testLocation
    enemy1 `spawnAt` location
    enemy2 `spawnAt` location
    self `moveTo` location
    investigator2 `moveTo` location
    self `putCardIntoPlay` Events.dynamiteBlast
    click "choose your location"
    replicateM_ 3 $ click "assign Damage"
    replicateM_ 3 $ click "assign Damage"
    enemy1.damage `shouldReturn` 3
    enemy2.damage `shouldReturn` 3
    self.damage `shouldReturn` 3
    investigator2.damage `shouldReturn` 3

  it
    "does 3 damage to each enemy and investigator at a connected location"
    $ gameTest
    $ \self -> do
      investigator2 <- addInvestigator Investigators.rolandBanks
      enemy1 <- testEnemy & prop @"health" 4
      enemy2 <- testEnemy & prop @"health" 4
      (location1, location2) <- testConnectedLocations id id
      enemy1 `spawnAt` location1
      enemy2 `spawnAt` location2
      self `moveTo` location1
      investigator2 `moveTo` location2
      self `putCardIntoPlay` Events.dynamiteBlast
      chooseTarget location2
      replicateM_ 3 $ click "assign Damage"
      enemy1.damage `shouldReturn` 0
      enemy2.damage `shouldReturn` 3
      self.damage `shouldReturn` 0
      investigator2.damage `shouldReturn` 3
