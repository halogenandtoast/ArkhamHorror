module Arkham.Event.Events.IllTakeThatSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "\"I'll take that!\"" do
  context "with Chuck Fergus (2) in play" do
    -- The Chuck Fergus reaction fires in the PlayCard window, which no longer
    -- carries the successful investigation. "I'll take that!" reads that window
    -- for its cost reduction, so re-checking its playability there used to fail
    -- and the trigger was presented as mandatory (see #5319).
    it "the reaction to playing it is still optional" . gameTest $ \self -> do
      withProp @"intellect" 4 self
      withProp @"resources" 1 self
      _ <- self `putAssetIntoPlay` Assets.chuckFergus2
      illTakeThat <- genCard Events.illTakeThat
      lockpicks <- genCard Assets.lockpicks
      self `addToHand` illTakeThat
      self `addToHand` lockpicks
      location <- testLocation & prop @"shroud" 2 & prop @"clues" 1
      self `moveTo` location
      setChaosTokens [Zero]
      self `investigate` location
      startSkillTest
      applyResults
      chooseTarget illTakeThat
      skip
      chooseTarget lockpicks
      assertAny $ assetIs Assets.lockpicks
