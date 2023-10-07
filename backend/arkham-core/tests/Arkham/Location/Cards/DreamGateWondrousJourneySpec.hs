module Arkham.Location.Cards.DreamGateWondrousJourneySpec (spec) where

import Arkham.Investigator.Cards (lukeRobinson, rolandBanks)
import Arkham.Location.Cards qualified as Locations
import TestImport.New

spec :: Spec
spec = describe "Dream Gate (Wondrous Journey)" do
  it "Dream-Gate is connected to each other revealed location, and vice versa"
    . gameTestWith lukeRobinson
    $ \self -> do
      (location1, location2) <- testConnectedLocations id id
      (dreamGate, placement) <- placeLocationCard Locations.dreamGateWondrousJourney
      self `moveTo` location1
      run placement
      location1.connectedLocations `shouldMatchListM` [dreamGate, toId location2]
      dreamGate.connectedLocations `shouldMatchListM` [toId location1]
      location2.connectedLocations `shouldMatchListM` [toId location1]

  it "Enemies and investigators other than Luke Robinson cannot enter Dream-Gate."
    . gameTestWith lukeRobinson
    $ \self -> do
      (location1, location2) <- testConnectedLocations id id
      (dreamGate, placement) <- placeLocationCard Locations.dreamGateWondrousJourney
      run placement
      self `moveTo` dreamGate
      enemy <- testEnemy & hunter
      enemy `spawnAt` location1
      run HuntersMove
      enemy.location `shouldReturn` Just (toId location1)
      roland <- addInvestigator rolandBanks
      roland `moveTo` location2
      roland.accessibleLocations `shouldReturn` [toId location1]

  context "At the end of the investigation phase" do
    it
      "Set Dream-Gate aside, out of play. (If Luke Robinson is here, move him to any revealed location.)"
      . gameTestWith lukeRobinson
      $ \self -> do
        (location1, location2) <- testConnectedLocations id id
        (dreamGate, placement) <- placeLocationCard Locations.dreamGateWondrousJourney
        run placement
        self `moveTo` location1
        duringPhase #investigation $ do
          self `moveTo` dreamGate
        useForcedAbility
        assertNotTarget location2
        chooseTarget location1
        self.location `shouldReturn` Just (toId location1)

    it
      "if there are no revealed locations, he is defeated"
      . gameTestWith lukeRobinson
      $ \self -> do
        (dreamGate, placement) <- placeLocationCard Locations.dreamGateWondrousJourney
        run placement
        duringPhase #investigation $ do
          self `moveTo` dreamGate
        useForcedAbility
        assert self.defeated
