module Arkham.Event.Events.OnTheHuntSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (EnemyPlacement))
import Arkham.Event.Cards qualified as Events
import Arkham.Matcher
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
