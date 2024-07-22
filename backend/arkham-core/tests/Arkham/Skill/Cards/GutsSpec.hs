module Arkham.Skill.Cards.GutsSpec (spec) where

import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Guts" $ do
  maxCommittedPerSkillTest 1 Skills.guts

  it "if this test is successful draw a card" . gameTest $ \self -> do
    cards <- testPlayerCards 1
    withProp @"deck" (Deck cards) self
    withProp @"willpower" 0 self
    guts <- genCard Skills.guts
    self `addToHand` guts
    withEach [(Zero, map toCard cards), (MinusOne, [])] $ \(token, expectedHand) -> do
      setChaosTokens [token]
      sid <- getRandom
      run $ beginSkillTest sid self #willpower 2
      commit guts
      startSkillTest
      applyResults
      self.hand `shouldReturn` expectedHand
