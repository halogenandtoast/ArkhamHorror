module Arkham.Event.Cards.BarricadeSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Modifier

spec :: Spec
spec = do
  describe "Barricade" $ do
    it "should make the current location unenterable by non elites" $ do
      location <- testLocation id
      investigator <- testInvestigator id
      barricade <- buildEvent "01038" investigator
      gameTest
          investigator
          [moveTo investigator location, playEvent investigator barricade]
          ((entitiesL . eventsL %~ insertEntity barricade)
          . (entitiesL . locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            getModifiers (TestSource mempty) (toTarget location)
              `shouldReturn` [CannotBeEnteredByNonElite]
            isAttachedTo location barricade `shouldReturn` True

    it "should be discarded if an investigator leaves the location" $ do
      (location1, location2) <- testConnectedLocations id id
      investigator <- testInvestigator id
      investigator2 <- testInvestigator id
      barricade <- buildEvent "01038" investigator
      gameTest
          investigator
          [ moveAllTo location1
          , playEvent investigator barricade
          , Move (toSource investigator2) (toId investigator2) (toId location1) (toId location2)
          ]
          ((entitiesL . eventsL %~ insertEntity barricade)
          . (entitiesL . locationsL %~ insertEntity location1)
          . (entitiesL . locationsL %~ insertEntity location2)
          . (entitiesL . investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            chooseOnlyOption "trigger barricade"
            getModifiers (TestSource mempty) (toTarget location1)
              `shouldReturn` []
            isAttachedTo location1 barricade `shouldReturn` False
            isInDiscardOf investigator barricade `shouldReturn` True
