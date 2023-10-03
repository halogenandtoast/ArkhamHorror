module Arkham.Event.Cards.WorkingAHunchSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Working a Hunch" $ do
  it "discovers a clue at your location" . gameTest $ \self -> do
    location <- testLocation & prop @"clues" 1
    self `moveTo` location
    duringTurn self $ do
      self `playEvent` Events.workingAHunch
      self.clues `shouldReturn` 1
      location.clues `shouldReturn` 0
