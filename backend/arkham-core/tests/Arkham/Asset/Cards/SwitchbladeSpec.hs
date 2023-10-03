module Arkham.Asset.Cards.SwitchbladeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Switchblade" $ do
  isFastAsset Assets.switchblade
  context "Fight action" $ do
    it "gives +1 damage if you succeed by 2 or more" . gameTest $ \self -> do
      withProp @"combat" 3 self
      location <- testLocation
      enemy <- testEnemy & prop @"health" 3 & prop @"fight" 1
      switchblade <- self `putAssetIntoPlay` Assets.switchblade
      self `moveTo` location
      enemy `spawnAt` location
      [doFight] <- self `getActionsFrom` switchblade

      withEach [(MinusOne, 1), (Zero, 2)] $ \(token, damage) -> do
        setChaosTokens [token]
        self `useAbility` doFight
        chooseTarget enemy
        startSkillTest
        applyResults
        enemy.damage `shouldReturn` damage
