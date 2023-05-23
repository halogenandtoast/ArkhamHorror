module Arkham.Event.Cards.Barricade3Spec (
  spec,
) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (eventIs)
import Arkham.Movement
import Arkham.Projection

spec :: Spec
spec = do
  describe "Barricade 3" $ do
    it
      "should make the current location unenterable by non elites and non elites cannot spawn there"
      $ gameTest
      $ \investigator -> do
        location <- testLocation id
        pushAndRun $ moveTo investigator location
        putCardIntoPlay investigator Events.barricade3
        getModifiers (toTarget location)
          `shouldReturn` [ CannotBeEnteredByNonElite
                         , SpawnNonEliteAtConnectingInstead
                         ]
        assert $ fieldPM LocationEvents (anyM (<=~> eventIs Events.barricade3) . setToList) (toId location)

    it "should be discarded if an investigator leaves the location" $ gameTest $ \investigator -> do
      location <- testLocation id
      investigator2 <- addInvestigator Investigators.rolandBanks id
      pushAndRun $ moveAllTo location
      putCardIntoPlay investigator Events.barricade3
      pushAndRun $ Move $ move investigator2 (toId investigator2) (toId location)
      chooseOnlyOption "trigger barricade"
      getModifiers (toTarget location)
        `shouldReturn` []
      assert $ fieldP LocationEvents null (toId location)
      assert $ isInDiscardOf investigator Events.barricade3
