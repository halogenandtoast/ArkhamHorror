module Arkham.Event.Events.SeekingAnswersSpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Seeking Answers" $ do
  it "discovers 1 clue at a connecting location without Deduction inflating it (#4830)"
    . gameTest
    $ \self -> do
      withProp @"intellect" 2 self
      (home, connecting) <- testConnectedLocations id id
      updateProp @"shroud" 2 home
      updateProp @"clues" 0 home
      updateProp @"clues" 2 connecting
      deduction <- genCard Skills.deduction
      setChaosTokens [Zero]
      self `moveTo` home
      self `addToHand` deduction
      self `playEvent` Events.seekingAnswers
      commit deduction
      startSkillTest
      applyResults
      chooseTarget connecting
      -- Deduction must not add a clue at the connecting location (home has no clues to
      -- discover its bonus from), so only Seeking Answers' single clue is taken.
      connecting.clues `shouldReturn` 1
      home.clues `shouldReturn` 0
      self.clues `shouldReturn` 1

  it "lets Deduction discover its additional clue at the investigated location"
    . gameTest
    $ \self -> do
      withProp @"intellect" 2 self
      (home, connecting) <- testConnectedLocations id id
      updateProp @"shroud" 2 home
      updateProp @"clues" 2 home
      updateProp @"clues" 2 connecting
      deduction <- genCard Skills.deduction
      setChaosTokens [Zero]
      self `moveTo` home
      self `addToHand` deduction
      self `playEvent` Events.seekingAnswers
      commit deduction
      startSkillTest
      applyResults
      chooseTarget connecting
      -- Seeking Answers takes 1 clue from the connecting location, Deduction takes 1 from
      -- the investigated (home) location.
      connecting.clues `shouldReturn` 1
      home.clues `shouldReturn` 1
      self.clues `shouldReturn` 2
