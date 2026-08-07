module Arkham.Event.Events.ThePaintedWorldSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "The Painted World" do
  -- The played event is substituted into hand under The Painted World's card id.
  -- Its in-hand {reaction} abilities (here Intel Report's "increase its cost by
  -- 2") must still be offered at the play window.
  it "offers the played event's cost-increase reaction" . gameTest $ \self -> do
    withProp @"resources" 10 self
    location <- testLocation
    self `moveTo` location

    paintedWorld <- genMyCard self Events.thePaintedWorld
    intelReport <- genMyCard self Events.intelReport
    self `addToHand` paintedWorld
    run $ PlaceUnderneath (toTarget self) [intelReport]

    playCard self paintedWorld
    chooseTarget intelReport
    chooseOptionMatching "increase cost to discover 2 clues" \case
      AbilityLabel {ability} -> ability.index == 1
      _ -> False

    skip
    self.clues `shouldReturn` 2
    self.resources `shouldReturn` 6
