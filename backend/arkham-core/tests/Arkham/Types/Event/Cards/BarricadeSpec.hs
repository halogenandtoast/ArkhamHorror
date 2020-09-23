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
      (locationId, location) <- testLocation "00000" id
      (investigatorId, investigator) <- testInvestigator "00000" id
      (barricadeId, barricade) <- buildEvent "01038" investigatorId
      game <-
        runGameTest
          investigator
          [ MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId barricadeId
          ]
        $ (events %~ insertMap barricadeId barricade)
        . (locations %~ insertMap locationId location)
      location `shouldSatisfy` hasModifier game CannotBeEnteredByNonElite
      barricade `shouldSatisfy` isAttachedTo game location

    it "should be discarded if an investigator leaves the location" $ do
      (locationId, location) <- testLocation "00000" id
      (investigatorId, investigator) <- testInvestigator "00000" id
      (investigator2Id, investigator2) <- testInvestigator "00001" id
      (barricadeId, barricade) <- buildEvent "01038" investigatorId
      game <-
        runGameTest
          investigator
          [ MoveAllTo locationId
          , InvestigatorPlayEvent investigatorId barricadeId
          , MoveFrom investigator2Id locationId
          ]
        $ (events %~ insertMap barricadeId barricade)
        . (locations %~ insertMap locationId location)
        . (investigators %~ insertMap investigator2Id investigator2)
      location `shouldSatisfy` not . hasModifier game CannotBeEnteredByNonElite
      barricade `shouldSatisfy` not . isAttachedTo game location
      barricade `shouldSatisfy` isInDiscardOf game investigator
