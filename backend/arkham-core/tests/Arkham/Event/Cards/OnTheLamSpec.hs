module Arkham.Event.Cards.OnTheLamSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "On The Lam" $ do
  it "prevents non-elite enemies from attacking you until the end of the round" . gameTest $ \self -> do
    ref <- createMessageChecker \case
      EnemyAttack {} -> True
      _ -> False
    withProp @"resources" 1 self
    location <- testLocation
    enemy <- testEnemy
    enemy `spawnAt` location
    self `withHand` [Events.onTheLam]
    self `moveTo` location

    duringRound $ do
      duringTurn self $ do
        chooseFirstOption "Play on the lam"
        takeResource self
        ref `refShouldBe` False

      run EnemiesAttack
      ref `refShouldBe` False

    run EnemiesAttack
    ref `refShouldBe` True
