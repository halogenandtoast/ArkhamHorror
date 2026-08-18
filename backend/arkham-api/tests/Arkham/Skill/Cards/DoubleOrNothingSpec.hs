module Arkham.Skill.Cards.DoubleOrNothingSpec (spec) where

import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Double or Nothing" $ do
  it "resolves the results of a successful fight twice" . gameTest $ \self -> do
    withProp @"combat" 4 self
    enemy <- testEnemy & prop @"health" 10 & prop @"fight" 1
    location <- testLocation
    doubleOrNothing <- genCard Skills.doubleOrNothing
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    self `addToHand` doubleOrNothing
    void $ self `fightEnemy` enemy
    commit doubleOrNothing
    startSkillTest
    applyResults
    enemy.damage `shouldReturn` 2

  -- #5431: the ST.7 results are determined once and then resolved twice, so
  -- Vicious Blow's +1 damage must not be applied once per repeat.
  it "does not stack Vicious Blow's bonus damage across the repeat" . gameTest $ \self -> do
    withProp @"combat" 4 self
    enemy <- testEnemy & prop @"health" 10 & prop @"fight" 1
    location <- testLocation
    viciousBlow <- genCard Skills.viciousBlow
    doubleOrNothing <- genCard Skills.doubleOrNothing
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    self `addToHand` viciousBlow
    self `addToHand` doubleOrNothing
    void $ self `fightEnemy` enemy
    commit viciousBlow
    commit doubleOrNothing
    startSkillTest
    applyResults
    enemy.damage `shouldReturn` 4

  it "does not stack Deduction's bonus clue across the repeat" . gameTest $ \self -> do
    withProp @"intellect" 4 self
    location <- testLocation & prop @"clues" 5 & prop @"shroud" 1
    deduction <- genCard Skills.deduction
    doubleOrNothing <- genCard Skills.doubleOrNothing
    setChaosTokens [Zero]
    self `moveTo` location
    self `addToHand` deduction
    self `addToHand` doubleOrNothing
    self `investigate` location
    commit deduction
    commit doubleOrNothing
    startSkillTest
    applyResults
    self.clues `shouldReturn` 4
    location.clues `shouldReturn` 1

  it "still resolves a committed card's own effect twice" . gameTest $ \self -> do
    cards <- testPlayerCards 2
    withProp @"deck" (Deck cards) self
    withProp @"intellect" 4 self
    perception <- genCard Skills.perception
    doubleOrNothing <- genCard Skills.doubleOrNothing
    self `addToHand` perception
    self `addToHand` doubleOrNothing
    setChaosTokens [Zero]
    sid <- getRandom
    run $ beginSkillTest sid self #intellect 2
    commit perception
    commit doubleOrNothing
    startSkillTest
    applyResults
    self.hand `shouldReturn` map toCard cards
