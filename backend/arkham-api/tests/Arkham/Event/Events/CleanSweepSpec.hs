module Arkham.Event.Events.CleanSweepSpec (spec) where

import Arkham.Direction
import Arkham.Event.Cards qualified as Events
import Arkham.Location.Cards qualified as Locations
import TestImport.New

spec :: Spec
spec = describe "Clean Sweep" $ do
  it "offers a connecting location that its own clue discovery unblocks (#5315)"
    . gameTest
    $ \self -> do
      withProp @"intellect" 2 self
      -- Passenger Car #169 is blocked while it is unrevealed and the location to its left
      -- has any clues, so it only becomes enterable once Clean Sweep takes the last clue.
      (home, passengerCar) <-
        testConnectedLocationsWithDef (Locations.rivertown, id) (Locations.passengerCar_169, id)
      run $ PlacedLocationDirection (toId home) LeftOf (toId passengerCar)
      updateProp @"shroud" 1 home
      updateProp @"clues" 1 home
      setChaosTokens [Zero]
      self `moveTo` home
      self `playEvent` Events.cleanSweep
      startSkillTest
      applyResults
      chooseOptionMatching "discover the clue first" \case
        Label lbl _ -> "Discover Clue at" `isInfixOf` lbl
        _ -> False
      -- Clean Sweep's option is the only one left, so it runs without a further prompt
      clickLabel "$label.moveToConnecting"
      assertTarget passengerCar
