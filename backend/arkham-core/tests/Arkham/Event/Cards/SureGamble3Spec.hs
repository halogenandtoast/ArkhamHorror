module Arkham.Event.Cards.SureGamble3Spec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Sure Gamble (3)" $ do
  it "when you reveal a negative chaos token changes a - to a +" . gameTest $ \self -> do
    withProp @"intellect" 0 self
    withProp @"resources" 2 self
    sureGamble3 <- genCard Events.sureGamble3
    self `addToHand` sureGamble3
    setChaosTokens [MinusOne]
    run $ beginSkillTest self #intellect 1
    startSkillTest
    chooseTarget sureGamble3
    assertPassedSkillTest
