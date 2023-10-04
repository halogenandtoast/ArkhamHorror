module Arkham.Skill.Cards.OverpowerSpec (spec) where

import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Overpower" $ do
  maxCommittedPerSkillTest 1 Skills.overpower

  it "if this test is successful draw a card" . gameTest $ \self -> do
    cards <- testPlayerCards 1
    withProp @"deck" (Deck cards) self
    withProp @"combat" 0 self
    overpower <- genCard Skills.overpower
    self `addToHand` overpower
    withEach [(Zero, map toCard cards), (MinusOne, [])] $ \(token, expectedHand) -> do
      setChaosTokens [token]
      run $ beginSkillTest self #combat 2
      commit overpower
      startSkillTest
      applyResults
      self.hand `shouldReturn` expectedHand
