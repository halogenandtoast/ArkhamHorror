module Arkham.Treachery.Cards.SmiteTheWickedSpec (spec) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import TestImport.Lifted qualified as Msg
import TestImport.New

spec :: Spec
spec = describe "Smite the Wicked" $ do
  it "draws an enemy, attaches to it, and spawns farthest away from you" $ gameTest $ \investigator -> do
    enemy <- genEncounterCard Cards.swarmOfRats
    treachery <- genEncounterCard Cards.ancientEvils
    (location1, location2) <- testConnectedLocations id id
    pushAndRunAll [placedLocation location1, placedLocation location2]
    run $ SetEncounterDeck (Deck [treachery, enemy])
    loadDeck investigator [Cards.smiteTheWicked]
    moveTo investigator location1
    drawCards investigator 1
    enemyId <- selectJust AnyEnemy
    assert $ fieldP EnemyLocation (== Just (toId location2)) enemyId
    assert $ selectAny (TreacheryOnEnemy (EnemyWithId enemyId))

  it "causes 1 mental trauma if enemy not defeated" $ gameTest $ \investigator -> do
    enemy <- genEncounterCard Cards.swarmOfRats
    location <- testLocationWith id
    run $ SetEncounterDeck (Deck [enemy])
    loadDeck investigator [Cards.smiteTheWicked]
    moveTo investigator location
    drawCards investigator 1
    run $ EndOfGame Nothing
    chooseOnlyOption "trigger smite the wicked"
    fieldAssert InvestigatorMentalTrauma (== 1) investigator

  it "won't cause trauma if enemy is defeated" . gameTest $ \investigator -> do
    enemy <- genEncounterCard Cards.swarmOfRats
    location <- testLocationWith id
    run $ placedLocation location
    run $ SetEncounterDeck (Deck [enemy])
    loadDeck investigator [Cards.smiteTheWicked]
    moveTo investigator location
    drawCards investigator 1
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
    fieldAssert InvestigatorDiscard (elem Cards.smiteTheWicked . map toCardDef) investigator

  it "will cause trauma if player is eliminated" $ gameTest $ \investigator -> do
    enemy <- genEncounterCard Cards.swarmOfRats
    location <- testLocationWith id
    run $ placedLocation location
    run $ SetEncounterDeck (Deck [enemy])
    loadDeck investigator [Cards.smiteTheWicked]
    moveTo investigator location
    drawCards investigator 1
    run $ Resign (toId investigator)
    chooseOnlyOption "trigger smite the wicked"
    fieldAssert InvestigatorMentalTrauma (== 1) investigator
