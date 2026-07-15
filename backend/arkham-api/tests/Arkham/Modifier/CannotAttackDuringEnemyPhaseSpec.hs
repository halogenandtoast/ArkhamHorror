module Arkham.Modifier.CannotAttackDuringEnemyPhaseSpec (spec) where

import Arkham.Attack qualified as Attack
import Arkham.Phase
import TestImport.New

-- Cosmic Emissary (Fate of the Vale) reads "this enemy does not attack during
-- the enemy phase this round", which used to be modelled as a blanket
-- CannotAttack. That also suppressed attacks the card still owes outside the
-- enemy phase -- notably Sublimation's "the nearest Cosmic Emissary enemy
-- attacks you", which silently did nothing (#5150).
spec :: Spec
spec = describe "CannotAttackDuringEnemyPhase" do
  let
    setup self = do
      enemy <- testEnemy & prop @"healthDamage" 1
      location <- testLocation
      enemy `spawnAt` location
      self `moveTo` location
      pure enemy
    attackMessage self enemy = InvestigatorAssignDamage self.id (EnemyAttackSource $ toId enemy) DamageAny 1 0

  context "during the enemy phase" do
    -- control: proves the harness actually observes an enemy-phase attack, so
    -- the suppression test below cannot pass vacuously
    it "attacks when unmodified" . gameTest $ \self -> do
      enemy <- setup self
      run $ SetPhase EnemyPhase
      run EnemiesAttack
      chooseTarget enemy
      applyAllDamage
      self.damage `shouldReturn` 1

    it "does not attack when modified" . gameTest $ \self -> do
      enemy <- setup self
      run =<< gameModifier (TestSource mempty) (toTarget enemy) CannotAttackDuringEnemyPhase
      run $ SetPhase EnemyPhase
      run EnemiesAttack
      applyAllDamage
      self.damage `shouldReturn` 0

  context "outside the enemy phase" do
    -- the #5150 regression: Sublimation's initiateEnemyAttack must still land
    it "still attacks when modified" . gameTest $ \self -> do
      enemy <- setup self
      run =<< gameModifier (TestSource mempty) (toTarget enemy) CannotAttackDuringEnemyPhase
      run $ SetPhase InvestigationPhase
      assertRunsMessage (attackMessage self enemy)
        $ run
        $ InitiateEnemyAttack
        $ Attack.enemyAttack (toId enemy) (toId enemy) self.id

    it "does not attack when CannotAttack, which stays unconditional" . gameTest $ \self -> do
      enemy <- setup self
      run =<< gameModifier (TestSource mempty) (toTarget enemy) CannotAttack
      run $ SetPhase InvestigationPhase
      assertDoesNotRunMessage (attackMessage self enemy)
        $ run
        $ InitiateEnemyAttack
        $ Attack.enemyAttack (toId enemy) (toId enemy) self.id
