module Arkham.Event.Events.DodgeSpec (spec) where

import Arkham.Attack qualified as Attack
import Arkham.Enemy.CardDefs.TheDrownedCity.StarSpawn qualified as Enemies
import Arkham.EnemyLocation.Cards qualified as EnemyLocations
import Arkham.EnemyLocation.Types (enemyLocationAsEnemyId)
import Arkham.Event.Cards qualified as Cards
import Arkham.Phase
import Arkham.Placement (Placement (AtLocation))
import TestImport.New

spec :: Spec
spec = do
  describe "Dodge" $ do
    it "cancels the attack" . gameTest $ \self -> do
      withProp @"resources" 1 self
      enemy <- testEnemy & prop @"healthDamage" 1
      location <- testLocation
      dodge <- genCard Cards.dodge

      self `addToHand` dodge
      enemy `spawnAt` location
      self `moveTo` location
      enemy `attacks` self

      let attackMessage = InvestigatorAssignDamage self.id (EnemyAttackSource $ toId enemy) DamageAny 1 0

      withRewind $ assertRunsMessage attackMessage skip
      assertDoesNotRunMessage attackMessage $ chooseTarget dodge

    -- #6066: a cancelled attack never exhausts the enemy, so the broadcast
    -- 'Do EnemiesAttack' catch-all that closes framework step 3.3 handed the
    -- same enemy a second, uncancelled attack.
    it "cancels the attack for good during the enemy phase" . gameTest $ \self -> do
      withProp @"resources" 1 self
      enemy <- testEnemy & prop @"healthDamage" 1
      location <- testLocation
      dodge <- genCard Cards.dodge

      self `addToHand` dodge
      enemy `spawnAt` location
      self `moveTo` location

      run $ SetPhase EnemyPhase
      run EnemiesAttack
      chooseTarget enemy
      chooseTarget dodge
      applyAllDamage

      self.damage `shouldReturn` 0

    -- The guard above must not reach a targeted "attack again": relentless
    -- readies and attacks a second time precisely because the enemy already
    -- attacked this phase, cancelled or not.
    it "does not stop a relentless enemy's second attack" . gameTest $ \self -> do
      withProp @"resources" 1 self
      coralStarSpawn <- testEnemyWithDef Enemies.coralStarSpawn id & prop @"healthDamage" 1
      location <- testLocation
      dodge <- genCard Cards.dodge

      self `addToHand` dodge
      coralStarSpawn `spawnAt` location
      self `moveTo` location

      run $ SetPhase EnemyPhase
      run EnemiesAttack
      chooseTarget coralStarSpawn
      chooseTarget dodge
      applyAllDamage
      self.damage `shouldReturn` 0

      run RelentlessEnemiesAttack
      chooseTarget coralStarSpawn
      applyAllDamage
      self.damage `shouldReturn` 1

    it "cancels an enemy-location attack" . gameTest $ \self -> do
      withProp @"resources" 1 self
      location <- testLocation
      enemyLocation <- genCard EnemyLocations.livingBedroomHemlockHouse34
      dodge <- genCard Cards.dodge
      let enemyId = enemyLocationAsEnemyId $ EnemyLocationId location.id

      self `addToHand` dodge
      self `moveTo` location
      run $ PlaceEnemyLocation location.id enemyLocation
      run $ InitiateEnemyAttack $ Attack.enemyAttack enemyId enemyId self.id

      let attackMessage = InvestigatorAssignDamage self.id (EnemyAttackSource enemyId) DamageAny 1 1

      withRewind $ assertRunsMessage attackMessage skip
      assertDoesNotRunMessage attackMessage $ chooseTarget dodge

    -- An enemy attacking an asset "as if it were an engaged investigator"
    -- (Dogs of War's Key Locus) is a real attack, so Dodge can cancel it.
    it "cancels an attack against an asset at your location" . gameTest $ \self -> do
      withProp @"resources" 1 self
      enemy <- testEnemy & prop @"healthDamage" 1
      location <- testLocation
      asset <- testAsset id self
      dodge <- genCard Cards.dodge

      self `addToHand` dodge
      run $ PlaceAsset (toId asset) (AtLocation location.id)
      enemy `spawnAt` location
      self `moveTo` location
      run $ InitiateEnemyAttack $ Attack.enemyAttack (toId enemy) (toId enemy) (toId asset)

      let attackMessage = DealAssetDamageWithCheck (toId asset) (EnemyAttackSource $ toId enemy) 1 0 True

      withRewind $ assertRunsMessage attackMessage skip
      assertDoesNotRunMessage attackMessage $ chooseTarget dodge
