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
      (investigatorId, daisyWalker) <- newInvestigator "01002"
      (eventId, barricade) <- newEvent "01038" investigatorId
      game <-
        runGameTest
          daisyWalker
          [ MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId eventId
          ]
        $ (events %~ insertMap eventId barricade)
        . (locations %~ insertMap locationId study)
      study `shouldSatisfy` hasModifier game CannotBeEnteredByNonElite
      barricade `shouldSatisfy` isAttachedTo game study

    it "should be discarded if an investigator leaves the location" $ do
      (hallwayId, hallway) <- newLocation "01112"
      (daisyWalkerId, daisyWalker) <- newInvestigator "01002"
      (rolandBanksId, rolandBanks) <- newInvestigator "01001"
      (eventId, barricade) <- newEvent "01038" daisyWalkerId
      game <-
        runGameTest
          daisyWalker
          [ MoveAllTo hallwayId
          , InvestigatorPlayEvent daisyWalkerId eventId
          , MoveFrom rolandBanksId hallwayId
          ]
        $ (events %~ insertMap eventId barricade)
        . (locations %~ insertMap hallwayId hallway)
        . (investigators %~ insertMap rolandBanksId rolandBanks)
      hallway `shouldSatisfy` not . hasModifier game CannotBeEnteredByNonElite
      barricade `shouldSatisfy` not . isAttachedTo game hallway
      barricade `shouldSatisfy` isInDiscardOf game daisyWalker
