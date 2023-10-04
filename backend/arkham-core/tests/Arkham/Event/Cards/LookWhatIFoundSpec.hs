module Arkham.Event.Cards.LookWhatIFoundSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "\"Look what I found!\"" $ do
  context "after you fail a skill test by 2 or less while investigating" $ do
    it "discovers 2 clues at your location" . gameTest $ \self -> do
      withProp @"intellect" 0 self
      withProp @"resources" 2 self
      location <- testLocation & prop @"shroud" 2 & prop @"clues" 3
      lookWhatIFound <- genCard Events.lookWhatIFound
      self `moveTo` location
      self `addToHand` lookWhatIFound
      setChaosTokens [Zero]
      self `investigate` location
      startSkillTest
      applyResults
      chooseTarget lookWhatIFound
      self.clues `shouldReturn` 2
      location.clues `shouldReturn` 1
