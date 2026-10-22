module Arkham.Asset.Assets.SurvivalKnife2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher (pattern AnyInPlayEnemy)
import Arkham.Phase
import TestImport.New

spec :: Spec
spec = describe "Survival Knife (2)" do
  -- "When an enemy attacks you during the enemy phase (before resolving that
  -- attack)" -- killing the attacker in that interrupt means it never attacks.
  it "stops the attack when its reaction fight defeats the attacker" . gameTest $ \self -> do
    withProp @"combat" 2 self
    location <- testLocation
    enemy <- testEnemy & prop @"fight" 3 & prop @"health" 2 & prop @"healthDamage" 1
    survivalKnife <- self `putAssetIntoPlay` Assets.survivalKnife2
    setChaosTokens [Zero]

    self `moveTo` location
    enemy `spawnAt` location

    run $ SetPhase EnemyPhase
    enemy `attacks` self

    let attackMessage = InvestigatorAssignDamage self.id (EnemyAttackSource $ toId enemy) DamageAny 1 0

    withRewind $ assertRunsMessage attackMessage skip
    assertDoesNotRunMessage attackMessage do
      useReaction
      chooseOnlyOption "fight the attacking enemy"
      chooseOnlyOption "Start skill test"
      chooseOnlyOption "Apply Results"

    assert $ selectNone AnyInPlayEnemy
    self.damage `shouldReturn` 0
    assert survivalKnife.exhausted

  it "still resolves the attack when the reaction fight fails" . gameTest $ \self -> do
    withProp @"combat" 2 self
    location <- testLocation
    enemy <- testEnemy & prop @"fight" 3 & prop @"health" 2 & prop @"healthDamage" 1
    survivalKnife <- self `putAssetIntoPlay` Assets.survivalKnife2
    setChaosTokens [AutoFail]

    self `moveTo` location
    enemy `spawnAt` location

    run $ SetPhase EnemyPhase
    enemy `attacks` self

    useReaction
    chooseOnlyOption "fight the attacking enemy"
    chooseOnlyOption "Start skill test"
    chooseOnlyOption "Apply Results"
    applyAllDamage

    enemy.damage `shouldReturn` 0
    self.damage `shouldReturn` 1
    assert survivalKnife.exhausted
