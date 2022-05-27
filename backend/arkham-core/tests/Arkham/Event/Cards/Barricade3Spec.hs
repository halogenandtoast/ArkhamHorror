module Arkham.Event.Cards.Barricade3Spec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Modifier

spec :: Spec
spec = do
  describe "Barricade 3" $ do
    it
        "should make the current location unenterable by non elites and non elites cannot spawn there"
      $ do
          location <- testLocation id
          investigator <- testInvestigator id
          barricade3 <- buildEvent "50004" investigator
          gameTest
              investigator
              [moveTo investigator location, playEvent investigator barricade3]
              ((entitiesL . eventsL %~ insertEntity barricade3)
              . (entitiesL . locationsL %~ insertEntity location)
              )
            $ do
                runMessages
                getModifiers (TestSource mempty) (toTarget location)
                  `shouldReturn` [ CannotBeEnteredByNonElite
                                 , SpawnNonEliteAtConnectingInstead
                                 ]
                isAttachedTo location barricade3 `shouldReturn` True

    it "should be discarded if an investigator leaves the location" $ do
      location <- testLocation id
      investigator <- testInvestigator id
      investigator2 <- testInvestigator id
      barricade3 <- buildEvent "50004" investigator
      gameTest
          investigator
          [ moveAllTo location
          , playEvent investigator barricade3
          , moveFrom investigator2 location
          ]
          ((entitiesL . eventsL %~ insertEntity barricade3)
          . (entitiesL . locationsL %~ insertEntity location)
          . (entitiesL . investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            getModifiers (TestSource mempty) (toTarget location)
              `shouldReturn` []
            isAttachedTo location barricade3 `shouldReturn` False
            isInDiscardOf investigator barricade3 `shouldReturn` True
