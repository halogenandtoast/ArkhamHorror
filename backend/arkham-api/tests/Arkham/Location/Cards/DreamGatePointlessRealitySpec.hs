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

    -- #4822: in The City of Archives, Luke is a "Body of a Yithian" (YithianForm),
    -- which used to break investigatorIs lukeRobinson and trap him in the gate.
    it
      "Luke can still leave even while a Body of a Yithian"
      . gameTestWith lukeRobinson
      $ \self -> do
        (location1, _location2) <- testConnectedLocations id id
        (dreamGate, placement) <- placeLocationCard Locations.dreamGatePointlessReality
        run placement
        run $ BecomeYithian (toId self)
        self `moveTo` location1
        duringPhase #investigation $ do
          self `moveTo` dreamGate
        useForcedAbility
        chooseTarget location1
        self.location `shouldReturn` Just (toId location1)

    -- #4822 / FAQ 071: a "cannot move" effect (e.g. Entombed) must not strand Luke
    -- at no location; ignore it long enough to place him in a revealed location.
    it
      "relocates Luke instead of defeating him when he cannot move"
      . gameTestWith lukeRobinson
      $ \self -> do
        (location1, _location2) <- testConnectedLocations id id
        (dreamGate, placement) <- placeLocationCard Locations.dreamGatePointlessReality
        run placement
        self `moveTo` location1
        duringPhase #investigation $ do
          self `moveTo` dreamGate
        run =<< gameModifier (TestSource mempty) (toTarget self) CannotMove
        useForcedAbility
        chooseTarget location1
        self.location `shouldReturn` Just (toId location1)
        self.defeated `shouldReturn` False
