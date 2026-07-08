module Arkham.UltimatumsAndBoons.BoonOfTheChildSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Boon of the Child" $ do
  it "once per round you may play the topmost event of your discard, bottom-decking it" . gameTest $ \self -> do
    withUltimatumsAndBoons [BoonOfTheChild]
    eventA <- genPlayerCardWith Events.emergencyCache (setPlayerCardOwner (toId self))
    eventB <- genPlayerCardWith Events.emergencyCache (setPlayerCardOwner (toId self))
    withProp @"discard" [eventA, eventB] self
    withDeck self [Assets.flashlight]

    duringRound do
      asDefs self.playableCards `shouldReturn` [Events.emergencyCache]
      self `playCard` toCard eventA
      self.resources `shouldReturn` 3
      -- it goes to the bottom of the deck instead of the discard pile
      asDefs self.discard `shouldReturn` [Events.emergencyCache]
      asDefs self.deck `shouldReturn` [Assets.flashlight, Events.emergencyCache]
      -- and the permission is spent for the rest of the round
      getModifiers self `shouldContainM` [MetaModifier "usedBoonOfTheChild"]
      asDefs self.playableCards `shouldReturn` []

    duringRound do
      -- the permission returns next round
      getModifiers self `shouldNotContainM` [MetaModifier "usedBoonOfTheChild"]
      asDefs self.playableCards `shouldReturn` [Events.emergencyCache]
