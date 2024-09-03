module Arkham.Asset.Cards.PendantOfTheQueenSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Matcher hiding (RevealLocation)
import TestImport.New

spec :: Spec
spec = describe "Pendant of the Queen" do
  context "if this has no charges" do
    it "set is aside of out of play and shuffle 3 set-aside copies of Segment of Onyx into your deck" . gameTest $ \self -> do
      segmentsOfOnyx <- replicateM 3 $ genCard Assets.segmentOfOnyx1
      withProp @"bonded" segmentsOfOnyx self
      pendantOfTheQueen <- self `putAssetIntoPlay` Assets.pendantOfTheQueen
      run $ SpendUses GameSource (toTarget pendantOfTheQueen) Charge 3
      assertNone $ assetIs Assets.pendantOfTheQueen
      (unDeck <$> self.deck) `shouldMatchListM` onlyPlayerCards segmentsOfOnyx

  context "Fast ability" do
    context "Spend 1 charge and choose a revealed location and select one" do
      it "move to that location" . gameTest $ \self -> do
        (location1, location2) <- testConnectedLocations id id
        updateProp @"clues" 0 location1
        updateProp @"clues" 0 location2
        self `moveTo` location1
        run $ RevealLocation Nothing (toId location2)
        pendantOfTheQueen <- self `putAssetIntoPlay` Assets.pendantOfTheQueen
        [usePendant] <- self `getActionsFrom` pendantOfTheQueen
        self `useAbility` usePendant
        self.location `shouldReturn` Just (toId location2)

      it "discover 1 clue at that location" . gameTest $ \self -> do
        (location1, location2) <- testConnectedLocations id id
        updateProp @"clues" 0 location1
        updateProp @"clues" 1 location2
        self `moveTo` location1
        run $ RevealLocation Nothing (toId location2)
        run $ gameModifier (TestSource mempty) location2 Blocked
        pendantOfTheQueen <- self `putAssetIntoPlay` Assets.pendantOfTheQueen
        [usePendant] <- self `getActionsFrom` pendantOfTheQueen
        self `useAbility` usePendant
        self.clues `shouldReturn` 1
        location2.clues `shouldReturn` 0

      it "automatically evade an enemy at that location" . gameTest $ \self -> do
        (location1, location2) <- testConnectedLocations id id
        updateProp @"clues" 0 location1
        updateProp @"clues" 0 location2
        enemy <- testEnemy
        enemy `spawnAt` location2
        self `moveTo` location1
        run $ RevealLocation Nothing (toId location2)
        run $ gameModifier (TestSource mempty) location2 Blocked
        pendantOfTheQueen <- self `putAssetIntoPlay` Assets.pendantOfTheQueen
        [usePendant] <- self `getActionsFrom` pendantOfTheQueen
        self `useAbility` usePendant
        assert enemy.exhausted
