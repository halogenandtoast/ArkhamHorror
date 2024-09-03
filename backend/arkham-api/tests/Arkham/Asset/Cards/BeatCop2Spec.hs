module Arkham.Asset.Cards.BeatCop2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Beat Cop (2)" $ do
  gives @"combat" Assets.beatCop2 1

  context "Exhaust Beat Cop and deal 1 damage to it" $ do
    it "Deal 1 damage to an enemy at your location." . gameTest $ \self -> do
      withProp @"combat" 0 self
      beatCop2 <- self `putAssetIntoPlay` Assets.beatCop2
      enemy <- testEnemy & prop @"health" 2
      location <- testLocation
      run $ placedLocation location
      enemy `spawnAt` location
      self `moveTo` location
      inWindow self $ useFastActionOf beatCop2 1
      enemy.damage `shouldReturn` 1
      beatCop2.damage `shouldReturn` 1
      assert beatCop2.exhausted
      self.discard `shouldReturn` []
