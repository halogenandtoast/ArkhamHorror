module Arkham.Asset.Assets.AncientAnkhSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Ancient Ankh" do
  it "makes the investigator fail by 1 instead" . gameTest $ \self -> do
    withProp @"willpower" 1 self
    location <- testLocation
    self `moveTo` location
    self `putCardIntoPlay` Assets.ancientAnkh

    didFailBy1 <- didFailSkillTestBy self #willpower 1

    setChaosTokens [Zero]
    sid <- getRandom
    -- willpower 1 against difficulty 4, so a failure by 3
    runSkillTest sid self #willpower 4
    assertFailedSkillTest
    useReaction
    applyResults
    didFailBy1 `refShouldBe` True
