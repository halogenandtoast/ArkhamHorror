{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Arkham.Event.Cards.BarricadeSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types (Field(..))
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Types (Field(..))
import Arkham.Projection
import Arkham.Movement

spec :: Spec
spec = do
  describe "Barricade" $ do
    it "should make the current location unenterable by non elites" $ do
      location <- testLocation id
      investigator <- testJenny id
      barricade <- buildEvent Events.barricade investigator
      gameTest
          investigator
          [moveTo investigator location, playEvent investigator barricade]
          ((entitiesL . eventsL %~ insertEntity barricade)
          . (entitiesL . locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            getModifiers (toTarget location)
              `shouldReturn` [CannotBeEnteredByNonElite]
            assert $ fieldP LocationEvents (== setFromList [toId barricade]) (toId location)
            assert $ fieldP InvestigatorDiscard null (toId investigator)

    it "should be discarded if an investigator leaves the location" $ do
      (location1, location2) <- testConnectedLocations id id
      investigator <- testJenny id
      investigator2 <- testInvestigator Investigators.rolandBanks id
      barricade <- buildEvent Events.barricade investigator
      let Just barricadeCard = preview _PlayerCard (toCard $ toAttrs barricade)
      gameTest
          investigator
          [ moveAllTo location1
          , playEvent investigator barricade
          , Move $ move investigator2 (toId investigator2) (toId location2)
          ]
          ((entitiesL . eventsL %~ insertEntity barricade)
          . (entitiesL . locationsL %~ insertEntity location1)
          . (entitiesL . locationsL %~ insertEntity location2)
          . (entitiesL . investigatorsL %~ insertEntity investigator2)
          )
        $ do
            runMessages
            chooseOnlyOption "trigger barricade"
            getModifiers (toTarget location1)
              `shouldReturn` []
            assert $ fieldP LocationEvents null (toId location1)
            assert $ fieldP InvestigatorDiscard (== [barricadeCard]) (toId investigator)
