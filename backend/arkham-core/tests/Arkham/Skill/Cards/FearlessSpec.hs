module Arkham.Skill.Cards.FearlessSpec (spec) where

import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Fearless" $ do
  it "heals 1 horror if the skill test is successful" . gameTest $ \self -> do
    withProp @"willpower" 0 self
    withProp @"horror" 1 self
    fearless <- genCard Skills.fearless
    self `addToHand` fearless
    withEach [(Zero, 0), (MinusOne, 1)] $ \(token, expectedHorror) -> do
      setChaosTokens [token]
      run $ beginSkillTest self #willpower 1
      commit fearless
      startSkillTest
      applyResults
      self.horror `shouldReturn` expectedHorror
