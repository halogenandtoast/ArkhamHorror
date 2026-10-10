module Arkham.Event.Events.LiveAndLearnSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Live and Learn" $ do
  it "repeats the test with +2 skill value" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    location <- testLocation & prop @"shroud" 3 & prop @"clues" 1
    setChaosTokens [Zero]
    self `moveTo` location
    liveAndLearn <- genCard Events.liveAndLearn
    self `addToHand` liveAndLearn
    investigate self location
    startSkillTest
    applyResults
    -- 1 intellect vs shroud 3, failed by 2
    location.clues `shouldReturn` 1
    chooseTarget liveAndLearn
    startSkillTest
    applyResults
    -- 1 intellect + 2 vs shroud 3, succeeds
    location.clues `shouldReturn` 0

  it "Drawing Thin's difficulty increase carries over to the repeated test" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    location <- testLocation & prop @"shroud" 3 & prop @"clues" 1
    setChaosTokens [Zero]
    self `moveTo` location
    drawingThin <- self `putAssetIntoPlay` Assets.drawingThin
    liveAndLearn <- genCard Events.liveAndLearn
    self `addToHand` liveAndLearn
    investigate self location
    useReactionOf drawingThin
    clickLabel "$label.takeResources count=i:2.0"
    startSkillTest
    applyResults
    -- 1 intellect vs shroud 3 + Drawing Thin 2, failed by 4
    location.clues `shouldReturn` 1
    chooseTarget liveAndLearn
    startSkillTest
    applyResults
    -- 1 intellect + 2 vs difficulty 5: the increase is inherent to the test, so
    -- it must still apply on the repeat and the investigation fails again
    location.clues `shouldReturn` 1

  it "is in the discard pile before the repeated test begins" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    location <- testLocation & prop @"shroud" 3 & prop @"clues" 1
    setChaosTokens [Zero]
    self `moveTo` location
    liveAndLearn <- genCard Events.liveAndLearn
    self `addToHand` liveAndLearn
    investigate self location
    startSkillTest
    applyResults
    chooseTarget liveAndLearn
    -- the event repeats the test, it does not create it, so it has finished
    -- resolving by the time the repeat is under way
    asDefs self.discard `shouldReturn` [Events.liveAndLearn]
    startSkillTest
    applyResults
    location.clues `shouldReturn` 0

  it "a second copy answers the original failure after the repeat resolves" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    location <- testLocation & prop @"shroud" 3 & prop @"clues" 2
    setChaosTokens [Zero]
    self `moveTo` location
    liveAndLearn1 <- genCard Events.liveAndLearn
    liveAndLearn2 <- genCard Events.liveAndLearn
    self `addToHand` liveAndLearn1
    self `addToHand` liveAndLearn2
    investigate self location
    startSkillTest
    applyResults
    -- 1 intellect vs shroud 3, failed by 2
    location.clues `shouldReturn` 2
    chooseTarget liveAndLearn1
    asDefs self.discard `shouldReturn` [Events.liveAndLearn]
    -- the repeat is nested in the window the first copy was declared in, so it resolves
    -- first: 1 intellect + 2 vs shroud 3 succeeds and discovers a clue
    startSkillTest
    applyResults
    location.clues `shouldReturn` 1
    -- then that window continues. The original failure is still its trigger, so the
    -- second copy can answer it even though the repeat passed
    chooseTarget liveAndLearn2
    startSkillTest
    applyResults
    location.clues `shouldReturn` 0

  it "the repeat's own failure window comes before the original's" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    location <- testLocation & prop @"shroud" 6 & prop @"clues" 1
    setChaosTokens [Zero]
    self `moveTo` location
    liveAndLearn1 <- genCard Events.liveAndLearn
    liveAndLearn2 <- genCard Events.liveAndLearn
    self `addToHand` liveAndLearn1
    self `addToHand` liveAndLearn2
    investigate self location
    startSkillTest
    applyResults
    -- 1 intellect vs shroud 6, failed by 5
    chooseTarget liveAndLearn1
    startSkillTest
    applyResults
    -- 1 intellect + 2 vs shroud 6 fails too. The repeat is the innermost sequence, so its
    -- own window is offered first: this skip is what that window needs, and the original's
    -- is still there behind it for the second copy
    skipAcrossQuestions
    chooseTarget liveAndLearn2
    asDefs self.discard `shouldMatchListM` [Events.liveAndLearn, Events.liveAndLearn]
    -- the skip leaves the seat declining windows, so the second repeat runs without
    -- stopping at its own commit step
    location.clues `shouldReturn` 1
