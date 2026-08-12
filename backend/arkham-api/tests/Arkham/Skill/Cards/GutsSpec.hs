module Arkham.Skill.Cards.GutsSpec (spec) where

import Arkham.Investigator.Cards (rolandBanks)
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

  context "Core 2026 (12090)" $ do
    it "the performing investigator draws the card when committed by another" . gameTest $ \self -> do
      let gutsDef = fromJustNote "guts (12090)" $ lookupCardDef ("12090" :: CardCode)
      selfCards <- testPlayerCards 1
      withProp @"deck" (Deck selfCards) self
      withProp @"willpower" 0 self
      other <- addInvestigator rolandBanks
      location <- testLocation
      self `moveTo` location
      other `moveTo` location
      guts <- genMyCard other gutsDef
      other `addToHand` guts
      setChaosTokens [Zero]
      sid <- getRandom
      run $ beginSkillTest sid self #willpower 2
      commitFor other guts
      startSkillTest
      applyResults
      self.hand `shouldReturn` map toCard selfCards
      other.hand `shouldReturn` []
