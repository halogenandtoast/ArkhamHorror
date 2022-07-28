{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Arkham.Event.Cards.Barricade3Spec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types (Field(..))
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Types (Field(..))
import Arkham.Projection

spec :: Spec
spec = do
  describe "Barricade 3" $ do
    it
        "should make the current location unenterable by non elites and non elites cannot spawn there"
      $ do
          location <- testLocation id
          investigator <- testJenny id
          barricade3 <- buildEvent Events.barricade3 investigator
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
                assert $ fieldP LocationEvents (== setFromList [toId barricade3]) (toId location)

    it "should be discarded if an investigator leaves the location" $ do
      location <- testLocation id
      investigator <- testJenny id
      investigator2 <- testInvestigator Investigators.rolandBanks id
      barricade3 <- buildEvent Events.barricade3 investigator
      let Just barricade3Card = preview _PlayerCard (toCard $ toAttrs barricade3)
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
            assert $ fieldP LocationEvents null (toId location)
            assert $ fieldP InvestigatorDiscard (== [barricade3Card]) (toId investigator)
