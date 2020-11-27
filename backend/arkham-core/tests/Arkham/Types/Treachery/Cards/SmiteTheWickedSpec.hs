module Arkham.Types.Treachery.Cards.SmiteTheWickedSpec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = describe "Smite the Wicked" $ do
  it "draws an enemy, attaches to it, and spawns farthest away from you" $ do
    investigator <- testInvestigator "00000" id
    smiteTheWicked <- buildPlayerCard "02007"
    enemy <- buildTestEnemyEncounterCard
    treachery <- buildTestTreacheryEncounterCard
    (location1, location2) <- testConnectedLocations id id
    game <-
      runGameTest
          investigator
          [ PlacedLocation (getLocationId location1)
          , PlacedLocation (getLocationId location2)
          , SetEncounterDeck [treachery, enemy]
          , loadDeck investigator [smiteTheWicked]
          , drawCards investigator 1
          ]
          ((locations %~ insertEntity location1)
          . (locations %~ insertEntity location2)
          )
        >>= runGameTestOnlyOption "place enemy"
    let updatedEnemy = game ^?! enemies . to toList . ix 0
    updated game location2 `shouldSatisfy` hasEnemy game updatedEnemy
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
          [ PlacedLocation (getLocationId location)
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
        (getEnemyId updatedEnemy)
        (getInvestigatorId investigator)
        (getLocationId location)
        (getCardCode enemy)
        (toSource investigator)
        []
      , EndOfGame
      ]

    updated game' investigator `shouldSatisfy` hasTrauma (0, 0)
    smiteTheWicked `shouldSatisfy` isInDiscardOf game' investigator

  it "will cause trauma if player is eliminated" $ do
    investigator <- testInvestigator "00000" id
    smiteTheWicked <- buildPlayerCard "02007"
    enemy <- buildTestEnemyEncounterCard
    location <- testLocation "00000" id
    game <-
      runGameTest
          investigator
          [ PlacedLocation (getLocationId location)
          , SetEncounterDeck [enemy]
          , loadDeck investigator [smiteTheWicked]
          , drawCards investigator 1
          , Resign (getInvestigatorId investigator)
          ]
          (locations %~ insertEntity location)
        >>= runGameTestOnlyOption "place enemy"

    updated game investigator `shouldSatisfy` hasTrauma (0, 1)
