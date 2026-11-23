module Arkham.Enemy.Cards.ReturnToTheDunwichLegacy.ReturnToTheEssexCountyExpress.TheConductorBeastFromBeyondTheGateSpec (
  spec,
) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.ReturnToTheDunwichLegacy.ReturnToTheEssexCountyExpress qualified as Enemies
import Arkham.Matcher
import Arkham.Phase
import Arkham.Placement
import TestImport.New

spec :: Spec
spec = describe "The Conductor (Beast from beyond the Gate)" do
  {- "Forced - When The Conductor is defeated: Attach it to the current agenda." That
  replaces the /disposal/, not the defeat, so an attack it had not resolved yet still must
  not resolve -- Survival Knife (2) prints "(before resolving that attack)". The replaced
  disposal cancels the `Do (Defeated ...)` that sets `enemyDefeated`, which is the guard
  `PerformEnemyAttack` reads, so the attack used to land anyway (#6050). -}
  it "does not resolve its attack when Survival Knife (2) defeats it in the interrupt"
    . gameTest
    $ \self -> do
      withProp @"combat" 2 self
      agenda <- testAgenda "51026" id
      location <- testLocation
      conductor <-
        testEnemyWithDef Enemies.theConductorBeastFromBeyondTheGate id
          & prop @"fight" 3
          & prop @"health" 2
          & prop @"healthDamage" 1
      survivalKnife <- self `putAssetIntoPlay` Assets.survivalKnife2
      setChaosTokens [Zero]

      self `moveTo` location
      conductor `spawnAt` location

      run $ SetPhase EnemyPhase
      conductor `attacks` self

      let attackMessage = InvestigatorAssignDamage self.id (EnemyAttackSource $ toId conductor) DamageAny 1 0

      withRewind $ assertRunsMessage attackMessage skip
      assertDoesNotRunMessage attackMessage do
        useReaction
        chooseOnlyOption "fight the attacking enemy"
        chooseOnlyOption "Start skill test"
        chooseOnlyOption "Apply Results"
        useForcedAbility

      self.damage `shouldReturn` 0
      self.horror `shouldReturn` 0
      assert survivalKnife.exhausted
      -- it left the board rather than the discard pile, but it still left it
      assertAny
        $ enemyIs Enemies.theConductorBeastFromBeyondTheGate
        <> EnemyWithPlacement (AttachedToAgenda $ toId agenda)
