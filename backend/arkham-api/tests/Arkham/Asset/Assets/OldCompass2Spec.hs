module Arkham.Asset.Assets.OldCompass2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.GameEnv (getJustSkillTest)
import Arkham.Modifier
import TestImport.New

spec :: Spec
spec = describe "Old Compass (2)" do
  it "reduces a location's shroud after its base value is set" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    oldCompass <- self `putAssetIntoPlay` Assets.oldCompass2
    location <- testLocation & prop @"shroud" 0 & prop @"clues" 1
    self `moveTo` location
    setChaosTokens [Zero]

    [investigateAction] <- self `getActionsFrom` oldCompass
    self `useAbility` investigateAction
    skillTest <- getJustSkillTest
    run =<< skillTestModifier skillTest.id (TestSource mempty) location (SetShroud 2)
    startSkillTest

    assertPassedSkillTest
