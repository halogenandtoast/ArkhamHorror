module Arkham.Event.Events.TrapLimitSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

-- A trap printed with "Limit 1 trap per location" can never share a location
-- with another trap: it cannot be played where a trap already sits, and once it
-- is attached no other trap (limited or not) may be played there.
--
-- Decoy Trap (60562) carries @LimitPerTraitPerLocation Trap 1@.
-- Snare Trap (03199) is a trap with no such limit.
spec :: Spec
spec = describe "Trap limit (1 trap per location)" do
  it "a limited trap cannot be played where a trap already sits" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    withProp @"resources" 10 self
    self `putCardIntoPlay` Events.snareTrap2
    decoy <- genPlayerCardWith Events.decoyTrap (setPlayerCardOwner (toId self))
    withProp @"hand" [toCard decoy] self
    duringTurn self do
      asDefs self.playableCards `shouldReturn` []

  it "an unlimited trap cannot be played onto a limited trap" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    withProp @"resources" 10 self
    self `putCardIntoPlay` Events.decoyTrap
    snare <- genPlayerCardWith Events.snareTrap2 (setPlayerCardOwner (toId self))
    withProp @"hand" [toCard snare] self
    duringTurn self do
      asDefs self.playableCards `shouldReturn` []

  it "either trap is playable at a location with no traps" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    withProp @"resources" 10 self
    decoy <- genPlayerCardWith Events.decoyTrap (setPlayerCardOwner (toId self))
    snare <- genPlayerCardWith Events.snareTrap2 (setPlayerCardOwner (toId self))
    withProp @"hand" [toCard decoy, toCard snare] self
    duringTurn self do
      asDefs self.playableCards `shouldMatchListM` [Events.decoyTrap, Events.snareTrap2]
