module Arkham.Event.Cards.LuckySpec (spec) where

import Arkham.Event.Cards qualified as Cards
import TestImport.New

spec :: Spec
spec = describe "Lucky!" $ do
  it "adds 2 to a skill test when you would fail" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    withProp @"resources" 1 self
    lucky <- genCard Cards.lucky

    setChaosTokens [MinusOne]
    self `addToHand` lucky
    runSkillTest self #intellect 2
    assertFailedSkillTest
    chooseTarget lucky
    assertPassedSkillTest

  it "does not cause an autofail to pass" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    withProp @"resources" 1 self
    lucky <- genCard Cards.lucky

    setChaosTokens [AutoFail]
    self `addToHand` lucky
    runSkillTest self #intellect 2
    assertFailedSkillTest
    chooseTarget lucky
    assertFailedSkillTest
