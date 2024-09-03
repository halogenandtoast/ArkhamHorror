module Arkham.Asset.Cards.GateBoxSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (lukeRobinson)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Gate Box" do
  hasUses @"charge" Assets.gateBox 3

  context "Fast Action" do
    it
      "Exhaust Gate Box and spend 1 charge: Disengage from each enemy engaged with you, search your bonded cards for Dream-Gate (Wondrous Journey), put it into play, and move to it."
      . gameTestWith lukeRobinson
      $ \self -> do
        gateBox <- self `putAssetIntoPlay` Assets.gateBox
        location <- testLocation
        self `moveTo` location
        enemy <- testEnemy
        enemy `spawnAt` location
        [useGate] <- self `getActionsFrom` gateBox
        self `useAbility` useGate
        dreamGateWondroudJourney <- selectJust $ locationIs Locations.dreamGateWondrousJourney
        self.location `shouldReturn` Just dreamGateWondroudJourney
        enemy.location `shouldReturn` Just (toId location)
        gateBox.charges `shouldReturn` 2
