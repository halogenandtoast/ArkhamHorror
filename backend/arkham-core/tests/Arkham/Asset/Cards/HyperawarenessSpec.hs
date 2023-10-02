module Arkham.Asset.Cards.HyperawarenessSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Hyperawareness" $ do
  it "Adds 1 to intellect check for each resource spent" . gameTest $ \self -> do
    withProp @"intellect" 0 self
    withProp @"resources" 2 self
    hyperawareness <- self `putAssetIntoPlay` Assets.hyperawareness
    run $ beginSkillTest self #intellect 3
    useFastActionOf hyperawareness 1
    useFastActionOf hyperawareness 1
    self.intellect `shouldReturn` 2

  it "Adds 1 to agility check for each resource spent" . gameTest $ \self -> do
    withProp @"agility" 0 self
    withProp @"resources" 2 self
    hyperawareness <- self `putAssetIntoPlay` Assets.hyperawareness
    run $ beginSkillTest self #agility 3
    useFastActionOf hyperawareness 2
    useFastActionOf hyperawareness 2
    self.agility `shouldReturn` 2
