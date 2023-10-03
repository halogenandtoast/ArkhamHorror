module Arkham.Skill.Cards.DeductionSpec (spec) where

import Arkham.Skill.Cards qualified as Cards
import TestImport.New

spec :: Spec
spec = describe "Deduction" $ do
  it "it allows you to discover another clue if you succeed" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    location <- testLocation & prop @"clues" 2 & prop @"shroud" 2
    deduction <- genCard Cards.deduction
    setChaosTokens [Zero]
    self `moveTo` location
    self `addToHand` deduction
    self `investigate` location
    commit deduction
    startSkillTest
    applyResults
    self.clues `shouldReturn` 2
    location.clues `shouldReturn` 0
