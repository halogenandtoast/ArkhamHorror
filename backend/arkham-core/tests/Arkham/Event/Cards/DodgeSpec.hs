module Arkham.Event.Cards.DodgeSpec (spec) where

import Arkham.Attack qualified as Attack
import Arkham.Event.Cards qualified as Cards
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

      let attackMessage = PerformEnemyAttack (toId enemy)

      withRewind $ assertRunsMessage attackMessage skip
      assertDoesNotRunMessage attackMessage $ chooseTarget dodge
