module Arkham.Asset.Cards.ScavengingSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Scavenging" $ do
  context "After you successfully investigate by 2 or more" $ do
    it "can return an Item asset to your hand" . gameTest $ \self -> do
      withProp @"intellect" 2 self
      magnifyingGlass <- genPlayerCard Assets.magnifyingGlass
      self `addToHand` (toCard magnifyingGlass)
      self `putCardIntoPlay` Assets.scavenging
      flashlight <- genPlayerCard Assets.flashlight
      beatCop <- genPlayerCard Assets.beatCop
      withProp @"discard" [flashlight, beatCop] self
      location <- testLocation & prop @"shroud" 0
      self `moveTo` location
      setChaosTokens [Zero]
      self `investigate` location
      commit magnifyingGlass
      startSkillTest
      applyResults
      useReaction
      assertNotTarget magnifyingGlass
      assertNotTarget beatCop
      chooseTarget flashlight
      self.hand `shouldReturn` [toCard flashlight]
      self.discard `shouldMatchListM` [beatCop, magnifyingGlass]
