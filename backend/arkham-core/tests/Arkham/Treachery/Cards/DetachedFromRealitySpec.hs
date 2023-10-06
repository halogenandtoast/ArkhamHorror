module Arkham.Treachery.Cards.DetachedFromRealitySpec (spec) where

import Arkham.Investigator.Cards (lukeRobinson)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Detached from Reality" do
  context "If Dream-Gate (Wondrous Journey) is already in play" do
    it "flips it over, disengages you from enemy and moves you to it" . gameTestWith lukeRobinson $ \self -> do
      (dreamGate, placement) <- placeLocationCard Locations.dreamGateWondrousJourney
      run placement
      location <- testLocation
      self `moveTo` location
      enemy <- testEnemy
      enemy `spawnAt` location
      self `drawsCard` Treacheries.detachedFromReality
      enemy.location `shouldReturn` Just (toId location)
      assertNone $ LocationWithFullTitle "Dream-Gate" "Wonderous Journey"
      assertAny $ LocationWithFullTitle "Dream-Gate" "Pointless Reality"
      self.location `shouldReturn` Just dreamGate

  context "Otherwise search bonded for Dream-Gate (Pointless Reality) and put it into play" do
    it "flips it over, disengages you from enemy and moves you to it" . gameTestWith lukeRobinson $ \self -> do
      location <- testLocation
      self `moveTo` location
      enemy <- testEnemy
      enemy `spawnAt` location
      self `drawsCard` Treacheries.detachedFromReality
      enemy.location `shouldReturn` Just (toId location)
      dreamGate <- selectJust $ locationIs Locations.dreamGatePointlessReality
      self.location `shouldReturn` Just dreamGate
