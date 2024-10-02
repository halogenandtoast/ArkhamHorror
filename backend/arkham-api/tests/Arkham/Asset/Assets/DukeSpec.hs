module Arkham.Asset.Assets.DukeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Locations
import TestImport.New hiding (EnemyDamage)

spec :: Spec
spec = describe "Duke" $ do
  context "fight action" $ do
    it "uses a base combat skill of 4 and does +1 damage" . gameTest $ \self -> do
      withProp @"combat" 1 self
      enemy <- testEnemy & prop @"health" 3 & prop @"fight" 4
      duke <- self `putAssetIntoPlay` Assets.duke
      location <- testLocation
      setChaosTokens [Zero]
      enemy `spawnAt` location
      self `moveTo` location
      [doFight, _] <- self `getActionsFrom` duke
      self `useAbility` doFight
      click "Fight enemy"
      click "Start skill test"
      click "Apply Results"
      enemy.damage `shouldReturn` 2

  context "investigate action" $ do
    it "uses a base intellect skill of 4" . gameTest $ \self -> do
      withProp @"intellect" 1 self
      duke <- self `putAssetIntoPlay` Assets.duke
      location <- testLocation & prop @"shroud" 4 & prop @"clues" 1
      setChaosTokens [Zero]
      self `moveTo` location
      duringTurn self do
        [investigateAction] <- self `getActionsFrom` duke
        self `useAbility` investigateAction
        click "Investigate current location"
        click "Start skill test"
        click "Apply results"
      self.clues `shouldReturn` 1

    it "you may move to a connecting location immediately before investigating" . gameTest $ \self -> do
      withProp @"intellect" 1 self
      duke <- self `putAssetIntoPlay` Assets.duke
      (location1, location2) <- testConnectedLocations id id
      updateProp @"shroud" 4 location2
      updateProp @"clues" 1 location2
      setChaosTokens [Zero]
      self `moveTo` location1
      [investigateAction] <- self `getActionsFrom` duke
      self `useAbility` investigateAction
      chooseTarget location2
      click "Start skill test"
      click "Apply results"
      self.clues `shouldReturn` 1

  context "Additional investigate costs" do
    it "must be paid them when the investigate portion happens" . gameTest $ \self -> do
      withProp @"remainingActions" 3 self
      duke <- self `putAssetIntoPlay` Assets.duke
      (currentLocation, orneLibrary) <-
        testConnectedLocationsWithDef (defaultTestLocation, id) (Locations.orneLibrary, id)
      self `moveTo` currentLocation
      [investigateAction] <- self `getActionsFrom` duke
      self `useAbility` investigateAction
      chooseTarget orneLibrary
      self.remainingActions `shouldReturn` 1

    it "will not be paid before moving" . gameTest $ \self -> do
      withProp @"remainingActions" 3 self
      duke <- self `putAssetIntoPlay` Assets.duke
      (destination, orneLibrary) <-
        testConnectedLocationsWithDef (defaultTestLocation, id) (Locations.orneLibrary, id)
      self `moveTo` orneLibrary
      [investigateAction] <- self `getActionsFrom` duke
      self `useAbility` investigateAction
      chooseTarget destination
      self.remainingActions `shouldReturn` 2
