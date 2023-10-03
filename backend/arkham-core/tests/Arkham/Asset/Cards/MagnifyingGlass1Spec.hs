module Arkham.Asset.Cards.MagnifyingGlass1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Magnifying Glass (1)" $ do
  isFastAsset Assets.magnifyingGlass1
  it "gives +1 intellect while investigating" . gameTest $ \self -> do
    withProp @"intellect" 0 self
    self `putCardIntoPlay` Assets.magnifyingGlass1
    location <- testLocation & prop @"shroud" 1
    self `moveTo` location
    self `investigate` location
    startSkillTest
    assertPassedSkillTest

  context "if there are no clues on your location" do
    it "can be returned to your hand" . gameTest $ \self -> do
      location1 <- testLocation & prop @"clues" 1
      location2 <- testLocation & prop @"clues" 0
      magnifyingGlass1 <- self `putAssetIntoPlay` Assets.magnifyingGlass1
      duringTurn self $ do
        self `moveTo` location1
        (self `getActionsFrom` magnifyingGlass1) `shouldReturn` []
        self `moveTo` location2
        [returnToHand] <- self `getActionsFrom` magnifyingGlass1
        self `useAbility` returnToHand
        asDefs self.hand `shouldReturn` [Assets.magnifyingGlass1]
        assert $ selectNone $ assetIs Assets.magnifyingGlass1
