module Arkham.Asset.Cards.ArcaneStudiesSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Arcane Studies" $ do
  it "Adds 1 to willpower check for each resource spent" . gameTest $ \self -> do
    withProp @"willpower" 0 self
    withProp @"resources" 2 self
    arcaneStudies <- self `putAssetIntoPlay` Assets.arcaneStudies
    run $ beginSkillTest self #combat 3 -- skill test type is irrelevant
    useFastActionOf arcaneStudies 1
    useFastActionOf arcaneStudies 1
    self.willpower `shouldReturn` 2

  it "Adds 1 to intellect check for each resource spent" . gameTest $ \self -> do
    withProp @"intellect" 0 self
    withProp @"resources" 2 self
    arcaneStudies <- self `putAssetIntoPlay` Assets.arcaneStudies
    run $ beginSkillTest self #combat 3 -- skill test type is irrelevant
    useFastActionOf arcaneStudies 2
    useFastActionOf arcaneStudies 2
    self.intellect `shouldReturn` 2
