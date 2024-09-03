module Arkham.Asset.Cards.HardKnocksSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Hard Knocks" $ do
  it "Adds 1 to combat check for each resource spent" . gameTest $ \self -> do
    withProp @"combat" 0 self
    withProp @"resources" 2 self
    hardKnocks <- self `putAssetIntoPlay` Assets.hardKnocks
    sid <- getRandom
    run $ beginSkillTest sid self #combat 3
    useFastActionOf hardKnocks 1
    useFastActionOf hardKnocks 1
    self.combat `shouldReturn` 2

  it "Adds 1 to agility check for each resource spent" . gameTest $ \self -> do
    withProp @"agility" 0 self
    withProp @"resources" 2 self
    hardKnocks <- self `putAssetIntoPlay` Assets.hardKnocks
    sid <- getRandom
    run $ beginSkillTest sid self #agility 3
    useFastActionOf hardKnocks 2
    useFastActionOf hardKnocks 2
    self.agility `shouldReturn` 2
