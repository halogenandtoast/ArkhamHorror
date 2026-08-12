module Arkham.Skill.Cards.ArroganceSpec (spec) where

import Arkham.Investigator.Cards (winifredHabbamock)
import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Arrogance" $ do
  it "can be committed to a skill test performed by another investigator at your location" . gameTest $ \self -> do
    withProp @"intellect" 2 self
    location <- testLocation & prop @"clues" 1 & prop @"shroud" 2
    other <- addInvestigator winifredHabbamock
    self `moveTo` location
    other `moveTo` location
    arrogance <- genMyCard other Skills.arrogance
    other `addToHand` arrogance
    setChaosTokens [Zero]
    self `investigate` location
    commitFor other arrogance
    startSkillTest
    applyResults
    -- 2 intellect less Arrogance's icon is 1, short of the shroud of 2
    self.clues `shouldReturn` 0
    location.clues `shouldReturn` 1
    other.hand `shouldReturn` []

  it "must be committed to each eligible skill test you perform" . gameTest $ \self -> do
    withProp @"intellect" 2 self
    location <- testLocation & prop @"clues" 1 & prop @"shroud" 1
    self `moveTo` location
    arrogance <- genMyCard self Skills.arrogance
    self `addToHand` arrogance
    setChaosTokens [Zero]
    self `investigate` location
    assertCannotStartSkillTest
    commit arrogance
    startSkillTest
    applyResults
    chooseFirstOption "resolve skill test options"
    -- 2 intellect less Arrogance's icon is 1, enough for the shroud of 1
    self.clues `shouldReturn` 1
    -- and it returns to hand because the test succeeded
    self.hand `shouldReturn` [arrogance]
