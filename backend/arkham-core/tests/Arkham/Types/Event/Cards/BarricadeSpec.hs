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
      location <- testLocation "00000" id
      investigator <- testInvestigator "00000" id
      barricade <- buildEvent "01038" investigator
      game <-
        runGameTest
          investigator
          [moveTo investigator location, playEvent investigator barricade]
        $ (events %~ insertEntity barricade)
        . (locations %~ insertEntity location)
      location `shouldSatisfy` hasModifier game CannotBeEnteredByNonElite
      barricade `shouldSatisfy` isAttachedTo game location

    it "should be discarded if an investigator leaves the location" $ do
      location <- testLocation "00000" id
      investigator <- testInvestigator "00000" id
      investigator2 <- testInvestigator "00001" id
      barricade <- buildEvent "01038" investigator
      game <-
        runGameTest
          investigator
          [ moveAllTo location
          , playEvent investigator barricade
          , moveFrom investigator2 location
          ]
        $ (events %~ insertEntity barricade)
        . (locations %~ insertEntity location)
        . (investigators %~ insertEntity investigator2)
      location `shouldSatisfy` not . hasModifier game CannotBeEnteredByNonElite
      barricade `shouldSatisfy` not . isAttachedTo game location
      barricade `shouldSatisfy` isInDiscardOf game investigator
