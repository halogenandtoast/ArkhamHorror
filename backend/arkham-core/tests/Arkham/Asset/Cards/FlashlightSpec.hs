module Arkham.Asset.Cards.FlashlightSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Flashlight" $ do
  hasUses @"supply" Assets.flashlight 3
  context "investigate action" $ do
    it "spend 1 supply, your location gets -2 shroud" . gameTest $ \self -> do
      withProp @"intellect" 1 self
      flashlight <- self `putAssetIntoPlay` Assets.flashlight
      location <- testLocation & prop @"shroud" 3
      self `moveTo` location
      setChaosTokens [Zero]
      [doInvestigate] <- self `getActionsFrom` flashlight
      self `useAbility` doInvestigate
      startSkillTest
      assertPassedSkillTest
