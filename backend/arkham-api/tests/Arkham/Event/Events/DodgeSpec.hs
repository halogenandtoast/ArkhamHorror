module Arkham.Event.Events.DodgeSpec (spec) where

import Arkham.Attack qualified as Attack
import Arkham.Event.Cards qualified as Cards
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
