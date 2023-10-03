module Arkham.Asset.Cards.BurglarySpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Burglary" $ do
  context "Investigate Action" $ do
    it "exhausts and if you succeed instead of gaining clues, gain 3 resources" . gameTest $ \self -> do
      withProp @"intellect" 1 self
      withProp @"resources" 0 self
      location <- testLocation & prop @"clues" 1 & prop @"shroud" 0
      setChaosTokens [Zero]
      self `moveTo` location
      burglary <- self `putAssetIntoPlay` Assets.burglary
      [doInvestigate] <- self `getActionsFrom` burglary
      self `useAbility` doInvestigate
      startSkillTest
      applyResults
      location.clues `shouldReturn` 1
      self.resources `shouldReturn` 3
