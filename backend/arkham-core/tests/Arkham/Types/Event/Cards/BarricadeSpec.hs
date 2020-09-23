module Arkham.Types.Event.Cards.BarricadeSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Modifier

spec :: Spec
spec = do
  describe "Barricade" $ do
    it "should make the current location unenterable by non elites" $ do
      (locationId, study) <- newLocation "01111"
      (investigatorId, investigator) <- newInvestigator "00000" id
      (eventId, barricade) <- newEvent "01038" investigatorId
      game <-
        runGameTest
          investigator
          [ MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId eventId
          ]
        $ (events %~ insertMap eventId barricade)
        . (locations %~ insertMap locationId study)
      study `shouldSatisfy` hasModifier game CannotBeEnteredByNonElite
      barricade `shouldSatisfy` isAttachedTo game study

    it "should be discarded if an investigator leaves the location" $ do
      (hallwayId, hallway) <- newLocation "01112"
      (investigatorId, investigator) <- newInvestigator "00000" id
      (investigator2Id, investigator2) <- newInvestigator "00001" id
      (eventId, barricade) <- newEvent "01038" investigatorId
      game <-
        runGameTest
          investigator
          [ MoveAllTo hallwayId
          , InvestigatorPlayEvent investigatorId eventId
          , MoveFrom investigator2Id hallwayId
          ]
        $ (events %~ insertMap eventId barricade)
        . (locations %~ insertMap hallwayId hallway)
        . (investigators %~ insertMap investigator2Id investigator2)
      hallway `shouldSatisfy` not . hasModifier game CannotBeEnteredByNonElite
      barricade `shouldSatisfy` not . isAttachedTo game hallway
      barricade `shouldSatisfy` isInDiscardOf game investigator
