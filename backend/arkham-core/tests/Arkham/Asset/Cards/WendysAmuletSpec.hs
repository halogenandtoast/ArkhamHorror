module Arkham.Asset.Cards.WendysAmuletSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Wendy's Amulet" $ do
  context "constant ability" $ do
    it "you may play the topmost event in your discard pile as if it were in your hand" . gameTest $ \self -> do
      self `putCardIntoPlay` Assets.wendysAmulet
      withProp @"discard" [Assets.flashlight, Events.emergencyCache] self
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
