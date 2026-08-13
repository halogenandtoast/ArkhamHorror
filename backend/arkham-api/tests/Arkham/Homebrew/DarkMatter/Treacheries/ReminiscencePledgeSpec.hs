module Arkham.Homebrew.DarkMatter.Treacheries.ReminiscencePledgeSpec (spec) where

import Arkham.Helpers.Message qualified as Helpers
import Arkham.Helpers.Scenario
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Treacheries
import Arkham.Matcher
import Arkham.Placement (Placement (HiddenInHand))
import Arkham.Scenario.Types
import TestImport.New

spec :: Spec
spec = describe "Reminiscence (Pledge)" do
  -- "heal all damage from it instead" — the Defeated message is queued behind
  -- the EnemyWouldBeDefeated window, so healing without cancelling the defeat
  -- still sends the enemy to the victory display.
  it "heals the enemy instead of it being defeated" . gameTest $ \self -> do
    withProp @"combat" 1 self
    enemy <- testEnemy & prop @"health" 1 & prop @"fight" 1
    location <- testLocation
    pledge <- genCard Treacheries.reminiscencePledge
    createPledge <- Helpers.createTreacheryAt_ pledge (HiddenInHand self.id)
    run createPledge
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    void $ self `fightEnemy` enemy
    startSkillTest
    applyResults
    useReaction
    assert $ selectAny $ EnemyWithId enemy.id
    enemy.damage `shouldReturn` 0
    scenarioField ScenarioVictoryDisplay `shouldSatisfyM` notElem (toCard enemy)
