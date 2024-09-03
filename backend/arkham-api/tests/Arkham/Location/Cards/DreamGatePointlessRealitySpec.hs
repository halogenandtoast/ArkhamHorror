module Arkham.Location.Cards.DreamGatePointlessRealitySpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards (lukeRobinson, rolandBanks)
import Arkham.Location.Cards qualified as Locations
import TestImport.New

spec :: Spec
spec = describe "Dream Gate (Pointless Reality)" do
  it "Enemies and investigators other than Luke Robinson cannot enter Dream-Gate."
    . gameTestWith lukeRobinson
    $ \self -> do
      (location1, location2) <- testConnectedLocations id id
      (dreamGate, placement) <- placeLocationCard Locations.dreamGatePointlessReality
      run placement
      self `moveTo` dreamGate
      enemy <- testEnemy & hunter
      enemy `spawnAt` location1
      run HuntersMove
      enemy.location `shouldReturn` Just (toId location1)
      roland <- addInvestigator rolandBanks & prop @"resources" 2
      self `moveTo` location2
      roland `moveTo` location2
      duringTurn roland do
        run HuntersMove
        run $ RevealLocation Nothing (toId location1)
        roland `playEvent` Events.elusive
        -- can only move to location 1
        roland.location `shouldReturn` Just (toId location1)

  context "At the end of the investigation phase" do
    it
      "Set Dream-Gate aside, out of play. (If Luke Robinson is here, move him to any revealed location.)"
      . gameTestWith lukeRobinson
      $ \self -> do
        (location1, location2) <- testConnectedLocations id id
        (dreamGate, placement) <- placeLocationCard Locations.dreamGatePointlessReality
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
        (dreamGate, placement) <- placeLocationCard Locations.dreamGatePointlessReality
        run placement
        duringPhase #investigation $ do
          self `moveTo` dreamGate
        useForcedAbility
        assert self.defeated
