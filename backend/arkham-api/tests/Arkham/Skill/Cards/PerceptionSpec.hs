module Arkham.Skill.Cards.PerceptionSpec (spec) where

import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Perception" $ do
  maxCommittedPerSkillTest 1 Skills.perception

  it "if this test is successful draw a card" . gameTest $ \self -> do
    cards <- testPlayerCards 1
    withProp @"deck" (Deck cards) self
    withProp @"intellect" 0 self
    perception <- genCard Skills.perception
    self `addToHand` perception
    withEach [(Zero, map toCard cards), (MinusOne, [])] $ \(token, expectedHand) -> do
      setChaosTokens [token]
      sid <- getRandom
      run $ beginSkillTest sid self #intellect 2
      commit perception
      startSkillTest
      applyResults
      self.hand `shouldReturn` expectedHand
