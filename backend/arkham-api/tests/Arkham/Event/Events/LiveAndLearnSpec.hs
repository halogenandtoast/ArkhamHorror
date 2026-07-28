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
