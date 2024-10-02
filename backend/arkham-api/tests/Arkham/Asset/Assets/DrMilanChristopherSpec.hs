module Arkham.Asset.Assets.DrMilanChristopherSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Dr. Milan Christopher" $ do
  gives @"intellect" Assets.drMilanChristopher 1
  context "After you successfully investigate" $ do
    it "Allows you to gain one resource" . gameTest $ \self -> do
      location <- testLocation & prop @"shroud" 0
      withProp @"resources" 0 self
      self `putCardIntoPlay` Assets.drMilanChristopher
      self `moveTo` location
      self `investigate` location
      startSkillTest
      applyResults
      useReaction
      self.resources `shouldReturn` 1

    it "Should only be triggerable once per successful investigation" . gameTest $ \self -> do
      location <- testLocation & prop @"shroud" 0
      withProp @"resources" 0 self
      self `putCardIntoPlay` Assets.drMilanChristopher
      self `moveTo` location
      self `investigate` location
      startSkillTest
      applyResults
      useReaction
      self.resources `shouldReturn` 1
      peekQueue `shouldReturn` []
