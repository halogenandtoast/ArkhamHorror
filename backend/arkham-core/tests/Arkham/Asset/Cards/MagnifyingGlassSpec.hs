module Arkham.Asset.Cards.MagnifyingGlassSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Magnifying Glass" $ do
  isFastAsset Assets.magnifyingGlass
  it "gives +1 intellect while investigating" . gameTest $ \self -> do
    withProp @"intellect" 0 self
    self `putCardIntoPlay` Assets.magnifyingGlass
    location <- testLocation & prop @"shroud" 1
    self `moveTo` location
    self `investigate` location
    startSkillTest
    assertPassedSkillTest
