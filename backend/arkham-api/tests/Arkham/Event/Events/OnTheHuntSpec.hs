module Arkham.Event.Events.OnTheHuntSpec (spec) where

import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Entities qualified as Entities
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyPlacement))
import Arkham.Event.Cards qualified as Events
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "On the Hunt" $ do
  it "spawns a chosen concealed enemy engaged with you instead of in the shadows" . gameTest $ \self -> do
    location <- testLocation
    coterieAgent <- genEncounterCard Enemies.coterieAgentA
    -- Keep another card in the deck so the search can shuffle the remainder back in.
    swarmOfRats <- genEncounterCard Enemies.swarmOfRats
    run $ SetEncounterDeck (Deck [coterieAgent, swarmOfRats])
    self `moveTo` location

    self `playEvent` Events.onTheHunt
    chooseTarget coterieAgent

    agent <- selectJust $ enemyIs Enemies.coterieAgentA
    field EnemyPlacement agent `shouldReturn` InThreatArea (toId self)
    assertNone $ EnemyWithPlacement InTheShadows
    assertNone ConcealedCardAny

  -- Regression for #4918: a scenario rule that overwrites an enemy's spawn
  -- location (Dead Heat's Gnashing Teeth applies OverwrittenSpawn
  -- SpawnAtRandomLocation to Ghoul/Risen enemies) must not override On the
  -- Hunt's "spawns engaged with you". On the Hunt's ForceSpawn takes precedence
  -- over the agenda's OverwrittenSpawn. The Ghoul is made Aloof so a plain
  -- location-spawn would not auto-engage, isolating the engagement to On the
  -- Hunt's override.
  it "spawns a chosen enemy engaged with you even when a scenario forces it to spawn at a random location" . gameTest $ \self -> do
    _ <- realAgenda Agendas.gnashingTeeth
    location <- testLocation
    ghoul <- genEncounterCard Enemies.ghoulMinion
    swarmOfRats <- genEncounterCard Enemies.swarmOfRats
    run $ SetEncounterDeck (Deck [ghoul, swarmOfRats])
    run =<< gameModifier (TestSource mempty) (toTarget ghoul) (AddKeyword Keyword.Aloof)
    self `moveTo` location

    self `playEvent` Events.onTheHunt
    chooseTarget ghoul

    minion <- selectJust $ enemyIs Enemies.ghoulMinion
    field EnemyPlacement minion `shouldReturn` InThreatArea (toId self)

realAgenda :: CardDef -> TestAppT ()
realAgenda def = do
  card <- genCard def
  let agenda' = lookupAgenda (AgendaId (toCardCode card)) 1 (toCardId card)
  overTest $ entitiesL . Entities.agendasL %~ insertEntity agenda'
