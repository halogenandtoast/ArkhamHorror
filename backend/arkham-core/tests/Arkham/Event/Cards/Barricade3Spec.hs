module Arkham.Event.Cards.Barricade3Spec (
  spec,
) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher (eventAt, eventIs)
import Arkham.Matcher.Patterns (pattern NonEliteEnemy)
import Arkham.Movement

spec :: Spec
spec = do
  describe "Barricade 3" $ do
    it
      "should make the current location unenterable by non elites and non elites cannot spawn there"
      $ gameTest
      $ \investigator -> do
        location <- testLocationWith id
        pushAndRun $ moveTo investigator location
        putCardIntoPlay investigator Events.barricade3
        getModifiers (toTarget location)
          `shouldReturn` [ CannotBeEnteredBy NonEliteEnemy
                         , SpawnNonEliteAtConnectingInstead
                         ]

        assert $ selectAny $ eventAt (toId location) <> eventIs Events.barricade3

    it "should be discarded if an investigator leaves the location" $ gameTest $ \investigator -> do
      location <- testLocationWith id
      investigator2 <- addInvestigator Investigators.rolandBanks id
      pushAndRun $ moveAllTo location
      putCardIntoPlay investigator Events.barricade3
      pushAndRun $ Move $ move investigator2 (toId investigator2) (toId location)
      chooseOnlyOption "trigger barricade"
      getModifiers (toTarget location)
        `shouldReturn` []
      assert $ selectNone $ eventAt (toId location) <> eventIs Events.barricade3
      assert $ Events.barricade3 `isInDiscardOf` investigator
