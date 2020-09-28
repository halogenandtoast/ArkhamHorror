module Arkham.Types.Treachery.Cards.SmiteTheWickedSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Location.Attrs as Location
import Arkham.Types.LocationSymbol

spec :: Spec
spec = describe "Smite the Wicked" $ do
  it "draws an enemy, attaches to it, and spawns farthest away from you" $ do
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
    updated game location2 `shouldSatisfy` hasEnemy updatedEnemy
    updatedEnemy `shouldSatisfy` hasTreacheryWithMatchingCardCode
      game
      (PlayerCard smiteTheWicked)

  it "causes 1 mental trauma if enemy not defeated" $ do
    investigator <- testInvestigator "00000" id
    smiteTheWicked <- buildPlayerCard "02007"
    enemy <- buildTestEnemyEncounterCard
    location <- testLocation "00000" id
    game <-
      runGameTest
          investigator
          [ SetEncounterDeck [enemy]
          , loadDeck investigator [smiteTheWicked]
          , drawCards investigator 1
          , EndOfGame
          ]
          (locations %~ insertEntity location)
        >>= runGameTestOnlyOption "place enemy"
    updated game investigator `shouldSatisfy` hasTrauma (0, 1)

  it "won't cause trauma if enemy is defeated" $ do
    investigator <- testInvestigator "00000" id
    smiteTheWicked <- buildPlayerCard "02007"
    enemy <- buildTestEnemyEncounterCard
    location <- testLocation "00000" id
    game <-
      runGameTest
          investigator
          [ PlacedLocation (getId () location)
          , SetEncounterDeck [enemy]
          , loadDeck investigator [smiteTheWicked]
          , drawCards investigator 1
          ]
          (locations %~ insertEntity location)
        >>= runGameTestOnlyOption "place enemy"
    let updatedEnemy = game ^?! enemies . to toList . ix 0

    game' <- runGameTestMessages
      game
      [ EnemyDefeated
        (getId () updatedEnemy)
        (getId () investigator)
        (getCardCode enemy)
        (InvestigatorSource (getId () investigator))
      , EndOfGame
      ]

    updated game' investigator `shouldSatisfy` hasTrauma (0, 0)
    smiteTheWicked `shouldSatisfy` isInDiscardOf game' investigator
