module Arkham.Asset.Assets.KeyOfMysteriesSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Placement (Placement (AtLocation))
import TestImport.New

spec :: Spec
spec = describe "Key of Mysteries" do
  it "gives control to the investigator after a successful investigation" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    location <- testLocation & prop @"clues" 1 & prop @"shroud" 0
    setChaosTokens [Zero]
    self `moveTo` location

    keyCard <- genCard Assets.keyOfMysteries
    key <- getRandom
    run $ CreateAssetAt key keyCard (AtLocation location.id)

    [doInvestigate] <- self `getActionsFrom` key
    self `useAbility` doInvestigate
    startSkillTest
    applyResults

    location.clues `shouldReturn` 1
    key.controller `shouldReturn` Just self.id
