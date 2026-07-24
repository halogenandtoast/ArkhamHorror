module Arkham.Treachery.Cards.StillBehindYouSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Helpers.Scenario
import Arkham.Scenario.Types (Field (ScenarioSetAsideCards))
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Still Behind You" do
  it "finds the set-aside Inescapable after a failed test" . gameTest $ \self -> do
    location <- testLocation
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

    assertAny $ enemyIs Enemies.theInescapable <> EnemyIsEngagedWith (InvestigatorWithId self.id)
    scenarioField ScenarioSetAsideCards `shouldReturn` []
