module Arkham.Skill.Cards.OpportunistSpec (spec) where

import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Opportunist" $ do
  it "if you succeed by 3 or more, return Opportunist to your hand" . gameTest $ \self -> do
    withProp @"intellect" 2 self
    opportunist <- genCard Skills.opportunist
    self `addToHand` opportunist
    withEach [(Zero, True), (MinusOne, False)] $ \(token, backToHand) -> do
      setChaosTokens [token]
      sid <- getRandom
      run $ beginSkillTest sid self #intellect 0
      commit opportunist
      startSkillTest
      applyResults
      self.hand `shouldReturn` [opportunist | backToHand]
      self.discard `shouldReturn` onlyPlayerCards [opportunist | not backToHand]
