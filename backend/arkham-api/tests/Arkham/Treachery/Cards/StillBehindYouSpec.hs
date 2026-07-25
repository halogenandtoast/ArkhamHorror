module Arkham.Treachery.Cards.StillBehindYouSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Scenario.Types (Field (ScenarioSetAsideCards))
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Still Behind You" do
  -- The Inescapable spawns at "any connecting location (empty, if able)", so the
  -- test needs somewhere for it to go: with a lone location it cannot spawn and
  -- is discarded instead.
  it "finds the set-aside Inescapable after a failed test" . gameTest $ \self -> do
    (location, connected) <- testConnectedLocations id id
    self `moveTo` location

    inescapable <- genCard Enemies.theInescapable
    stillBehindYou <- genEncounterCard Treacheries.stillBehindYou
    runAll
      [ SetAsideCards [inescapable]
      , SetEncounterDeck (Deck [stillBehindYou])
      ]
    setChaosTokens [AutoFail]

    run $ drawEncounterCard self.id GameSource
    startSkillTest
    applyResults

    assertAny $ enemyIs Enemies.theInescapable <> enemyAt connected.id
    scenarioField ScenarioSetAsideCards `shouldReturn` []
    assertAny $ VictoryDisplayCardMatch $ basic $ cardIs Treacheries.stillBehindYou
