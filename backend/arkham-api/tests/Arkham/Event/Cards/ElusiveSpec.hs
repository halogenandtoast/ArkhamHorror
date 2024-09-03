module Arkham.Event.Cards.ElusiveSpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Location.Types (revealedL)
import TestImport.New

spec :: Spec
spec = describe "Elusive" $ do
  it "Disengage from each enemy engaged with you and move to a revealed location with no enemies." . gameTest $ \self -> do
    enemy1 <- testEnemy
    enemy2 <- testEnemy
    enemy3 <- testEnemy
    (location1, location2) <- testConnectedLocations (revealedL .~ True) (revealedL .~ True)
    location3 <- testLocation & prop @"revealed" True
    self `moveTo` location1
    enemy1 `spawnAt` location1
    enemy2 `spawnAt` location1
    enemy3 `spawnAt` location2
    duringTurn self $ do
      self `playEvent` Events.elusive
      -- faq: You can move to any revealed location with no enemies, not necessarily a connecting one
      self.location `shouldReturn` Just (toId location3)
      -- faq: disengaging does not exhaust the enemy
      enemy1.exhausted `shouldReturn` False
      enemy2.exhausted `shouldReturn` False

  faq ":" $ do
    it "You can play Elusive to move to another location even if you're not engaged with any enemies" . gameTest $ \self -> do
      (location1, _) <- testConnectedLocations (revealedL .~ True) (revealedL .~ True)
      location3 <- testLocation & prop @"revealed" True
      self `moveTo` location1
      duringTurn self $ do
        self `playEvent` Events.elusive
        chooseTarget location3
        -- faq: You can move to any revealed location with no enemies, not necessarily a connecting one
        self.location `shouldReturn` Just (toId location3)
