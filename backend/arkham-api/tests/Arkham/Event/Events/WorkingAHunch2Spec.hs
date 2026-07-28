module Arkham.Event.Events.WorkingAHunch2Spec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Working a Hunch (2)" do
  -- Issue #5262: cdCriteria only checked the CannotDiscoverClues* modifiers, so
  -- the card was playable — and paid for — with nothing to discover anywhere.
  it "is not playable when no revealed location has clues" . gameTest $ \self -> do
    withProp @"resources" 5 self
    card <- genCard Events.workingAHunch2
    withProp @"hand" [card] self
    location <- testLocation & prop @"revealed" True & prop @"clues" 0
    self `moveTo` location
    duringTurn self do
      asDefs self.playableCards `shouldReturn` []

  it "is playable when some other revealed location has clues" . gameTest $ \self -> do
    withProp @"resources" 5 self
    card <- genCard Events.workingAHunch2
    withProp @"hand" [card] self
    here <- testLocation & prop @"revealed" True & prop @"clues" 0
    there <- testLocation & prop @"revealed" True & prop @"clues" 1
    self `moveTo` here
    duringTurn self do
      asDefs self.playableCards `shouldReturn` [Events.workingAHunch2]
      self `playCard` card
      chooseTarget there
      self.clues `shouldReturn` 1
      there.clues `shouldReturn` 0
