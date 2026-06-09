module Arkham.Asset.Assets.WendysAmuletSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Wendy's Amulet" $ do
  context "constant ability" $ do
    it "you may play the topmost event in your discard pile as if it were in your hand" . gameTest $ \self -> do
      self `putCardIntoPlay` Assets.wendysAmulet
      withPropM @"discard" (genPlayerCards [Assets.flashlight, Events.emergencyCache]) self
      asDefs self.playableCards `shouldReturn` [Events.emergencyCache]

  context "forced ability" $ do
    context "after you play an event" $ do
      it "places it on the bottom of you deck instead of in your discard" . gameTest $ \self -> do
        emergencyCache <- genCard Events.emergencyCache
        self `withDeck` [Assets.flashlight]
        self `addToHand` emergencyCache
        self `putCardIntoPlay` Assets.wendysAmulet
        self `playCard` emergencyCache
        -- useForcedAbility -- TODO: apparently after play should be before putting into discard
        self.discard `shouldReturn` []
        asDefs self.deck `shouldReturn` [Assets.flashlight, Events.emergencyCache]

    errata "or discard an event from play" $ do
      it "places it on the bottom of you deck instead of in your discard" . gameTest $ \self -> do
        (location1, location2) <- testConnectedLocations id id
        barricade <- genCard Events.barricade
        self `withDeck` [Assets.flashlight]
        self `addToHand` barricade
        self `moveTo` location1
        self `putCardIntoPlay` Assets.wendysAmulet
        self `playCard` barricade
        self `moveTo` location2
        useForcedAbility
        self.discard `shouldReturn` []
        asDefs self.deck `shouldReturn` [Assets.flashlight, Events.barricade]

  context "playing an event with an \"increase cost\" reaction from the discard" $ do
    -- Regression for #4764: an event played from the discard (e.g. via Parallel
    -- Wendy's Amulet) whose reaction cost is "increase its cost" was being
    -- resolved twice. In-discard entities receive both the InDiscard-wrapped and
    -- the raw message, so the Event runner's duplicate UseAbility handlers each
    -- pushed a payment; the second double-applied the cost increase and crashed
    -- with InvalidState "Can't afford cost". It must resolve exactly once.
    it "resolves the reaction (and pays its cost) only once" . gameTest $ \self -> do
      intelReport <- genPlayerCardWith Events.intelReport (setPlayerCardOwner (toId self))
      withProp @"resources" 4 self
      withProp @"discard" [intelReport] self
      self `putCardIntoPlay` Assets.wendysAmuletAdvanced
      location <- testLocation & prop @"clues" 2
      self `moveTo` location
      duringTurn self do
        self `playCard` toCard intelReport
        useReaction -- "increase its cost by 2: Change Discover 1 clue to Discover 2 clues"
        self.clues `shouldReturn` 2
        self.resources `shouldReturn` 0
        location.clues `shouldReturn` 0
