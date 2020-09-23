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
      (locationId, study) <- newLocation "01111"
      (investigatorId, investigator) <- testInvestigator "00000" id
      (eventId, barricade3) <- buildEvent "50004" investigatorId
      game <-
        runGameTest
          investigator
          [ MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId eventId
          ]
        $ (events %~ insertMap eventId barricade3)
        . (locations %~ insertMap locationId study)
      study `shouldSatisfy` hasModifier game CannotBeEnteredByNonElite
      study `shouldSatisfy` hasModifier game SpawnNonEliteAtConnectingInstead
      barricade3 `shouldSatisfy` isAttachedTo game study

    it "should be discarded if an investigator leaves the location" $ do
      (hallwayId, hallway) <- newLocation "01112"
      (investigatorId, investigator) <- testInvestigator "00000" id
      (investigator2Id, investigator2) <- testInvestigator "00001" id
      (eventId, barricade3) <- buildEvent "01038" investigatorId
      game <-
        runGameTest
          investigator
          [ MoveAllTo hallwayId
          , InvestigatorPlayEvent investigatorId eventId
          , MoveFrom investigator2Id hallwayId
          ]
        $ (events %~ insertMap eventId barricade3)
        . (locations %~ insertMap hallwayId hallway)
        . (investigators %~ insertMap investigator2Id investigator2)
      hallway `shouldSatisfy` not . hasModifier game CannotBeEnteredByNonElite
      hallway
        `shouldSatisfy` not
        . hasModifier game SpawnNonEliteAtConnectingInstead
      barricade3 `shouldSatisfy` not . isAttachedTo game hallway
      barricade3 `shouldSatisfy` isInDiscardOf game investigator
