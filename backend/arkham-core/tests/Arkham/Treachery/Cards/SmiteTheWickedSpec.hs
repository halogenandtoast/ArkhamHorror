module Arkham.Treachery.Cards.SmiteTheWickedSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Types (Field (..))
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import TestImport.Lifted qualified as Msg

spec :: Spec
spec = describe "Smite the Wicked" $ do
  it "draws an enemy, attaches to it, and spawns farthest away from you" $ gameTest $ \investigator -> do
    smiteTheWicked <- genPlayerCard Cards.smiteTheWicked
    enemy <- genEncounterCard Cards.swarmOfRats
    treachery <- genEncounterCard Cards.ancientEvils
    (location1, location2) <- testConnectedLocations id id
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll
      [ placedLocation location1
      , placedLocation location2
      , SetEncounterDeck (Deck [treachery, enemy])
      , loadDeck investigator [smiteTheWicked]
      , moveTo investigator location1
      , drawing
      ]
    enemyId <- selectJust AnyEnemy
    assert $ fieldP EnemyLocation (== Just (toId location2)) enemyId
    assert $ selectAny (TreacheryOnEnemy (EnemyWithId enemyId))

  it "causes 1 mental trauma if enemy not defeated" $ gameTest $ \investigator -> do
    smiteTheWicked <- genPlayerCard Cards.smiteTheWicked
    enemy <- genEncounterCard Cards.swarmOfRats
    location <- testLocationWith id
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll
      [ SetEncounterDeck (Deck [enemy])
      , loadDeck investigator [smiteTheWicked]
      , moveTo investigator location
      , drawing
      , EndOfGame Nothing
      ]
    chooseOnlyOption "trigger smite the wicked"
    fieldAssert InvestigatorMentalTrauma (== 1) investigator

  it "won't cause trauma if enemy is defeated" $ gameTest $ \investigator -> do
    smiteTheWicked <- genPlayerCard Cards.smiteTheWicked
    enemy <- genEncounterCard Cards.swarmOfRats
    location <- testLocationWith id
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll
      [ placedLocation location
      , SetEncounterDeck (Deck [enemy])
      , loadDeck investigator [smiteTheWicked]
      , moveTo investigator location
      , drawing
      ]
    enemyId <- selectJust AnyEnemy
    pushAndRunAll
      [ Msg.EnemyDefeated
          enemyId
          (toCardId enemy)
          (toSource investigator)
          []
      , EndOfGame Nothing
      ]
    fieldAssert InvestigatorMentalTrauma (== 0) investigator
    fieldAssert InvestigatorDiscard (elem smiteTheWicked) investigator

  it "will cause trauma if player is eliminated" $ gameTest $ \investigator -> do
    smiteTheWicked <- genPlayerCard Cards.smiteTheWicked
    enemy <- genEncounterCard Cards.swarmOfRats
    location <- testLocationWith id
    drawing <- drawCards (toId investigator) investigator 1
    pushAndRunAll
      [ placedLocation location
      , SetEncounterDeck (Deck [enemy])
      , loadDeck investigator [smiteTheWicked]
      , moveTo investigator location
      , drawing
      , Resign (toId investigator)
      ]
    chooseOnlyOption "trigger smite the wicked"
    fieldAssert InvestigatorMentalTrauma (== 1) investigator
