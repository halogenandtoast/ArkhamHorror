module Arkham.Event.Cards.OnTheLamSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "On The Lam" $ do
  it "prevents non-elite enemies from attacking you until the end of the round" . gameTest $ \self -> do
    ref <- createMessageChecker \case
      EnemyAttack {} -> True
      _ -> False
    self `gainResources` 1
    onTheLam <- genCard Events.onTheLam
    location <- testLocation
    self `addToHand` onTheLam
    self `moveTo` location
    run $ BeginTurn (toId self)
    chooseFirstOption "Play on the lam"
    enemy <- testEnemy
    enemy `spawnAt` location
    push $ TakeResources (toId self) 1 (toSource self) True
    ref `refShouldBe` False

    runAll [ChooseEndTurn (toId self), EnemiesAttack]
    ref `refShouldBe` False

    runAll [EndRound, EnemiesAttack]
    ref `refShouldBe` True
