module Arkham.Treachery.Cards.SmiteTheWickedSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Enemy.Attrs ( Field (..) )
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import TestImport.Lifted qualified as Msg

spec :: Spec
spec = describe "Smite the Wicked" $ do
  it "draws an enemy, attaches to it, and spawns farthest away from you" $ do
    investigator <- testJenny id
    smiteTheWicked <- genPlayerCard Cards.smiteTheWicked
    enemy <- genEncounterCard Cards.swarmOfRats
    treachery <- genEncounterCard Cards.ancientEvils
    (location1, location2) <- testConnectedLocations id id
    gameTest
        investigator
        [ placedLocation location1
        , placedLocation location2
        , SetEncounterDeck (Deck [treachery, enemy])
        , loadDeck investigator [smiteTheWicked]
        , moveTo investigator location1
        , drawCards investigator 1
        ]
        ((entitiesL . locationsL %~ insertEntity location1)
        . (entitiesL . locationsL %~ insertEntity location2)
        )
      $ do
          runMessages
          chooseOnlyOption "place enemy"
          enemyId <- selectJust AnyEnemy
          fieldP EnemyLocation (== Just (toId location2)) enemyId
            `shouldReturn` True
          selectAny (TreacheryOnEnemy (EnemyWithId enemyId)) `shouldReturn` True

  it "causes 1 mental trauma if enemy not defeated" $ do
    investigator <- testJenny id
    smiteTheWicked <- genPlayerCard Cards.smiteTheWicked
    enemy <- genEncounterCard Cards.swarmOfRats
    location <- testLocation id
    gameTest
        investigator
        [ SetEncounterDeck (Deck [enemy])
        , loadDeck investigator [smiteTheWicked]
        , moveTo investigator location
        , drawCards investigator 1
        , EndOfGame Nothing
        ]
        (entitiesL . locationsL %~ insertEntity location)
      $ do
          runMessages
          chooseOnlyOption "place enemy"
          chooseOnlyOption "trigger smite the wicked"
          fieldAssert InvestigatorMentalTrauma (== 1) investigator

  it "won't cause trauma if enemy is defeated" $ do
    investigator <- testJenny id
    smiteTheWicked <- genPlayerCard Cards.smiteTheWicked
    enemy <- genEncounterCard Cards.swarmOfRats
    location <- testLocation id
    gameTest
        investigator
        [ placedLocation location
        , SetEncounterDeck (Deck [enemy])
        , loadDeck investigator [smiteTheWicked]
        , moveTo investigator location
        , drawCards investigator 1
        ]
        (entitiesL . locationsL %~ insertEntity location)
      $ do
          runMessages
          chooseOnlyOption "place enemy"
          enemyId <- selectJust AnyEnemy
          pushAll
            [ Msg.EnemyDefeated
              enemyId
              (toId investigator)
              (toCardCode enemy)
              (toSource investigator)
              []
            , EndOfGame Nothing
            ]
          runMessages
          fieldAssert InvestigatorMentalTrauma (== 0) investigator
          fieldAssert InvestigatorDiscard (elem smiteTheWicked) investigator

  it "will cause trauma if player is eliminated" $ do
    investigator <- testJenny id
    smiteTheWicked <- genPlayerCard Cards.smiteTheWicked
    enemy <- genEncounterCard Cards.swarmOfRats
    location <- testLocation id
    gameTest
        investigator
        [ placedLocation location
        , SetEncounterDeck (Deck [enemy])
        , loadDeck investigator [smiteTheWicked]
        , moveTo investigator location
        , drawCards investigator 1
        , Resign (toId investigator)
        ]
        (entitiesL . locationsL %~ insertEntity location)
      $ do
          runMessages
          chooseOnlyOption "place enemy"
          chooseOnlyOption "trigger smite the wicked"
          fieldAssert InvestigatorMentalTrauma (== 1) investigator
