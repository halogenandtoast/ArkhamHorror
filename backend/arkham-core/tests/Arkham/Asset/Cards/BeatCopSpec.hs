module Arkham.Asset.Cards.BeatCopSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Beat Cop" $ do
  it "gives you +1 combat" . gameTest $ \self -> do
    withProp @"combat" 0 self
    self `putCardIntoPlay` Assets.beatCop
    self.combat `shouldReturn` 1

  it "can be discarded to do 1 damage to an enemy at your location" . gameTest $ \self -> do
    withProp @"combat" 0 self
    beatCop <- self `putAssetIntoPlay` Assets.beatCop
    enemy <- testEnemy & prop @"health" 2
    location <- testLocation
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy `spawnAt` location
    self `moveTo` location
    inWindow self $ useFastActionOf beatCop 1
    enemy.damage `shouldReturn` 1
