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
      location <- testLocation id
      investigator <- testInvestigator id
      investigator2 <- testInvestigator id
      barricade <- buildEvent "01038" investigator
      gameTest
          investigator
          [ moveAllTo location
          , playEvent investigator barricade
          , moveFrom investigator2 location
          ]
          ((entitiesL . eventsL %~ insertEntity barricade)
          . (entitiesL . locationsL %~ insertEntity location)
          . (entitiesL . investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            getModifiers (TestSource mempty) (toTarget location)
              `shouldReturn` []
            isAttachedTo location barricade `shouldReturn` False
            isInDiscardOf investigator barricade `shouldReturn` True
