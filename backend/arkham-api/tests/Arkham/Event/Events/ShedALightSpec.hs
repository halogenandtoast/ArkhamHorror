module Arkham.Event.Events.ShedALightSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Shed a Light" $ do
  it "can be replayed by Double, Double" . gameTest $ \self -> do
    withProp @"intellect" 2 self
    withProp @"resources" 4 self
    location <- testLocation & prop @"clues" 10 & prop @"shroud" 0
    setChaosTokens [Zero]
    self `moveTo` location
    doubleDouble <- self `putAssetIntoPlay` Assets.doubleDouble4
    shedALight <- genCard Events.shedALight
    self `addToHand` shedALight
    investigate self location
    startSkillTest
    chooseTarget shedALight
    chooseTarget location
    -- "after you play an event" must still see the skill test, so Double,
    -- Double can replay Shed a Light before the test resolves
    useReactionOf doubleDouble
    chooseTarget location
    applyResults
    self.resources `shouldReturn` 0
    doubleDouble.exhausted `shouldReturn` True
