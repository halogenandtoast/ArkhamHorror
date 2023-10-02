module Arkham.Event.Cards.BarricadeSpec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher (eventAt, eventIs)
import Arkham.Matcher.Patterns (pattern NonEliteEnemy)
import TestImport.New

spec :: Spec
spec = do
  describe "Barricade" $ do
    it "should make the current location unenterable by non elites" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      self `putCardIntoPlay` Events.barricade
      getModifiers location `shouldReturn` [CannotBeEnteredBy NonEliteEnemy]
      assert $ selectAny $ eventAt (toId location) <> eventIs Events.barricade
      self.discard `shouldReturn` []

    it "should be discarded if an investigator leaves the location" $ gameTest $ \self -> do
      (location1, location2) <- testConnectedLocations id id
      investigator2 <- addInvestigator Investigators.rolandBanks
      run $ moveAllTo location1
      self `putCardIntoPlay` Events.barricade
      investigator2 `moveTo` location2
      click "trigger barricade"
      getModifiers location1 `shouldReturn` []
      assert $ selectNone $ eventAt (toId location1) <> eventIs Events.barricade
      assert $ Events.barricade `isInDiscardOf` self
