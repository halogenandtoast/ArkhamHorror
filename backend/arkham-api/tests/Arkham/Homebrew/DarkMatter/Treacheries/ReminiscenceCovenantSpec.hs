module Arkham.Homebrew.DarkMatter.Treacheries.ReminiscenceCovenantSpec (spec) where

import Arkham.Helpers.Message qualified as Helpers
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Treacheries
import Arkham.Placement (Placement (HiddenInHand))
import TestImport.New

spec :: Spec
spec = describe "Reminiscence (Covenant)" do
  -- "that enemy immediately attacks them instead" — the reaction replaces the
  -- evasion, so the enemy must neither exhaust nor disengage, and the attack
  -- has to actually resolve (an exhausted enemy's attack is silently dropped
  -- by attackIsValid).
  it "replaces a successful evade with an attack from that enemy" . gameTest $ \self -> do
    enemy <- testEnemy & prop @"evade" 0
    location <- testLocation
    withProp @"agility" 1 self
    setChaosTokens [Zero]
    covenant <- genCard Treacheries.reminiscenceCovenant
    createCovenant <- Helpers.createTreacheryAt_ covenant (HiddenInHand self.id)
    run createCovenant
    self `moveTo` location
    enemy `spawnAt` location
    self `evadeEnemy` enemy
    startSkillTest
    applyResults
    assertRunsMessage (PerformEnemyAttack enemy.id) useReaction
    assert $ not <$> enemy.exhausted
