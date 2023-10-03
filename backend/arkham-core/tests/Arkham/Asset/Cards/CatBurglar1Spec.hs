module Arkham.Asset.Cards.CatBurglar1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Cat Burglar (1)" $ do
  gives @"agility" Assets.catBurglar1 1

  context "action" $ do
    it "exhausts itself, disengages you from all enemies and moves you to a connecting location" . gameTest $ \self -> do
      catBurglar1 <- self `putAssetIntoPlay` Assets.catBurglar1
      enemy1 <- testEnemy
      enemy2 <- testEnemy
      (location1, location2) <- testConnectedLocations id id
      self `moveTo` location1
      enemy1 `spawnAt` location1
      enemy2 `spawnAt` location1
      [action] <- self `getActionsFrom` catBurglar1
      self `useAbility` action
      chooseTarget location2

      self.location `shouldReturn` Just (toId location2)
      self.engagedEnemies `shouldReturn` []
