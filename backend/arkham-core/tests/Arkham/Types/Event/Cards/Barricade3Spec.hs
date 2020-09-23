module Arkham.Types.Event.Cards.Barricade3Spec
  ( spec
  )
where

import TestImport

import Arkham.Types.Modifier

spec :: Spec
spec = do
  describe "Barricade 3" $ do
    it "should make the current location unenterable by non elites" $ do
      (locationId, location) <- testLocation "00000" id
      (investigatorId, investigator) <- testInvestigator "00000" id
      (eventId, barricade3) <- buildEvent "50004" investigatorId
      game <-
        runGameTest
          investigator
          [ MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId eventId
          ]
        $ (events %~ insertMap eventId barricade3)
        . (locations %~ insertMap locationId location)
      location `shouldSatisfy` hasModifier game CannotBeEnteredByNonElite
      location `shouldSatisfy` hasModifier game SpawnNonEliteAtConnectingInstead
      barricade3 `shouldSatisfy` isAttachedTo game location

    it "should be discarded if an investigator leaves the location" $ do
      (locationId, location) <- testLocation "00000" id
      (investigatorId, investigator) <- testInvestigator "00000" id
      (investigator2Id, investigator2) <- testInvestigator "00001" id
      (eventId, barricade3) <- buildEvent "01038" investigatorId
      game <-
        runGameTest
          investigator
          [ MoveAllTo locationId
          , InvestigatorPlayEvent investigatorId eventId
          , MoveFrom investigator2Id locationId
          ]
        $ (events %~ insertMap eventId barricade3)
        . (locations %~ insertMap locationId location)
        . (investigators %~ insertMap investigator2Id investigator2)
      location `shouldSatisfy` not . hasModifier game CannotBeEnteredByNonElite
      location
        `shouldSatisfy` not
        . hasModifier game SpawnNonEliteAtConnectingInstead
      barricade3 `shouldSatisfy` not . isAttachedTo game location
      barricade3 `shouldSatisfy` isInDiscardOf game investigator
