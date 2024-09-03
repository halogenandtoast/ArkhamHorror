module Arkham.Asset.Cards.DigDeepSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Dig Deep" $ do
  it "Adds 1 to willpower check for each resource spent" . gameTest $ \self -> do
    withProp @"willpower" 0 self
    withProp @"resources" 2 self
    digDeep <- self `putAssetIntoPlay` Assets.digDeep
    sid <- getRandom
    run $ beginSkillTest sid self #willpower 3
    useFastActionOf digDeep 1
    useFastActionOf digDeep 1
    self.willpower `shouldReturn` 2

  it "Adds 1 to agility check for each resource spent" . gameTest $ \self -> do
    withProp @"agility" 0 self
    withProp @"resources" 2 self
    digDeep <- self `putAssetIntoPlay` Assets.digDeep
    sid <- getRandom
    run $ beginSkillTest sid self #agility 3
    useFastActionOf digDeep 2
    useFastActionOf digDeep 2
    self.agility `shouldReturn` 2
