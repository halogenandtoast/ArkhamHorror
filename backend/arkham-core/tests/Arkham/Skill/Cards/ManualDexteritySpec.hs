module Arkham.Skill.Cards.ManualDexteritySpec (spec) where

import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Manual Dexterity" $ do
  maxCommittedPerSkillTest 1 Skills.manualDexterity

  it "if this test is successful draw a card" . gameTest $ \self -> do
    cards <- testPlayerCards 1
    withProp @"deck" (Deck cards) self
    withProp @"agility" 0 self
    manualDexterity <- genCard Skills.manualDexterity
    self `addToHand` manualDexterity
    withEach [(Zero, map toCard cards), (MinusOne, [])] $ \(token, expectedHand) -> do
      setChaosTokens [token]
      run $ beginSkillTest self #agility 2
      commit manualDexterity
      startSkillTest
      applyResults
      self.hand `shouldReturn` expectedHand
