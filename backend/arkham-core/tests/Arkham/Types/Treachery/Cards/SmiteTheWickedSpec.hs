module Arkham.Types.Treachery.Cards.SmiteTheWickedSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Location.Attrs as Location
import Arkham.Types.LocationSymbol

spec :: Spec
spec = describe "Smite the Wicked" $ do
  it "attaches to an enemy and causes 1 mental trauma if not defeated" $ do
    investigator <- testInvestigator "00000" id
    smiteTheWicked <- buildPlayerCard "02007"
    enemy <- buildTestEnemyEncounterCard
    treachery <- buildTestTreacheryEncounterCard
    location1 <- testLocation
      "00000"
      ((Location.symbol .~ Square)
      . (Location.revealedSymbol .~ Square)
      . (Location.connectedSymbols .~ setFromList [Triangle])
      . (Location.revealedConnectedSymbols .~ setFromList [Triangle])
      )
    location2 <- testLocation
      "00001"
      ((Location.symbol .~ Triangle)
      . (Location.revealedSymbol .~ Triangle)
      . (Location.connectedSymbols .~ setFromList [Square])
      . (Location.revealedConnectedSymbols .~ setFromList [Square])
      )
    game <-
      runGameTest
          investigator
          [ PlacedLocation (getId () location1)
          , PlacedLocation (getId () location2)
          , SetEncounterDeck [treachery, enemy]
          , loadDeck investigator [smiteTheWicked]
          , drawCards investigator 1
          , EndOfGame
          ]
          ((locations %~ insertEntity location1)
          . (locations %~ insertEntity location2)
          )
        >>= runGameTestOnlyOption "place enemy"
    let updatedEnemy = game ^?! enemies . to toList . ix 0
    updated game investigator `shouldSatisfy` hasTrauma (0, 1)
    updated game location2 `shouldSatisfy` hasEnemy updatedEnemy
    updatedEnemy `shouldSatisfy` hasTreacheryWithMatchingCardCode
      game
      (PlayerCard smiteTheWicked)


  it "won't cause trauma if enemy is defeated" $ do
    investigator <- testInvestigator "00000" id
    smiteTheWicked <- buildPlayerCard "02007"
    enemy <- buildTestEnemyEncounterCard
    treachery <- buildTestTreacheryEncounterCard
    location1 <- testLocation
      "00000"
      ((Location.symbol .~ Square)
      . (Location.revealedSymbol .~ Square)
      . (Location.connectedSymbols .~ setFromList [Triangle])
      . (Location.revealedConnectedSymbols .~ setFromList [Triangle])
      )
    location2 <- testLocation
      "00001"
      ((Location.symbol .~ Triangle)
      . (Location.revealedSymbol .~ Triangle)
      . (Location.connectedSymbols .~ setFromList [Square])
      . (Location.revealedConnectedSymbols .~ setFromList [Square])
      )
    game <-
      runGameTest
          investigator
          [ PlacedLocation (getId () location1)
          , PlacedLocation (getId () location2)
          , SetEncounterDeck [treachery, enemy]
          , loadDeck investigator [smiteTheWicked]
          , drawCards investigator 1
          ]
          ((locations %~ insertEntity location1)
          . (locations %~ insertEntity location2)
          )
        >>= runGameTestOnlyOption "place enemy"
    let updatedEnemy = game ^?! enemies . to toList . ix 0

    game' <- runGameTestMessages
      game
      [ EnemyDefeated
          (getId () updatedEnemy)
          (getId () investigator)
          (getCardCode enemy)
          (InvestigatorSource (getId () investigator))
      ]

    updated game' investigator `shouldSatisfy` hasTrauma (0, 0)
