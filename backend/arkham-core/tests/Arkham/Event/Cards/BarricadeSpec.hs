module Arkham.Event.Cards.BarricadeSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher (eventIs)
import Arkham.Movement
import Arkham.Projection

spec :: Spec
spec = do
  describe "Barricade" $ do
    it "should make the current location unenterable by non elites" $ gameTest $ \investigator -> do
      location <- testLocation id
      pushAndRun $ moveTo investigator location
      putCardIntoPlay investigator Events.barricade
      getModifiers (toTarget location)
        `shouldReturn` [CannotBeEnteredBy NonEliteEnemies]
      assert $ fieldPM LocationEvents (anyM (<=~> eventIs Events.barricade) . setToList) (toId location)
      assert $ fieldP InvestigatorDiscard null (toId investigator)

    it "should be discarded if an investigator leaves the location" $ gameTest $ \investigator -> do
      (location1, location2) <- testConnectedLocations id id
      investigator2 <- addInvestigator Investigators.rolandBanks id
      pushAndRun $ moveAllTo location1
      putCardIntoPlay investigator Events.barricade
      pushAndRun $ Move $ move investigator2 (toId investigator2) (toId location2)
      chooseOnlyOption "trigger barricade"
      getModifiers (toTarget location1)
        `shouldReturn` []
      assert $ fieldP LocationEvents null (toId location1)
      assert $ isInDiscardOf investigator Events.barricade
