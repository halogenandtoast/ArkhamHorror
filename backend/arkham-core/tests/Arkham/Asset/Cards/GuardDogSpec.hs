module Arkham.Asset.Cards.GuardDogSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Guard Dog" $ do
  it "does 1 damage to the attacking enemy when damaged by the attack" . gameTest $ \self -> do
    enemy <- testEnemy & prop @"healthDamage" 1 & prop @"health" 2
    location <- testLocation
    guardDog <- self `putAssetIntoPlay` Assets.guardDog
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    run EnemiesAttack
    self `assignDamageTo` guardDog
    useReaction
    enemy.damage `shouldReturn` 1
