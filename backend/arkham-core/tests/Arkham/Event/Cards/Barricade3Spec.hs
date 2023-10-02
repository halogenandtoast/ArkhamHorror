module Arkham.Event.Cards.Barricade3Spec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher (eventAt, eventIs)
import Arkham.Matcher.Patterns (pattern NonEliteEnemy)
import TestImport.New

spec :: Spec
spec = do
  describe "Barricade 3" $ do
    it "should make the current location unenterable by non elites and non elites cannot spawn there" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      self `putCardIntoPlay` Events.barricade3
      getModifiers location
        `shouldReturn` [CannotBeEnteredBy NonEliteEnemy, SpawnNonEliteAtConnectingInstead]
      assert $ selectAny $ eventAt (toId location) <> eventIs Events.barricade3

    it "should be discarded if an investigator leaves the location" $ gameTest $ \self -> do
      location <- testLocation
      investigator2 <- addInvestigator Investigators.rolandBanks
      run $ moveAllTo location
      self `putCardIntoPlay` Events.barricade3
      investigator2 `moveTo` location
      click "trigger barricade"
      getModifiers location `shouldReturn` []
      assert $ selectNone $ eventAt (toId location) <> eventIs Events.barricade3
      assert $ Events.barricade3 `isInDiscardOf` self
