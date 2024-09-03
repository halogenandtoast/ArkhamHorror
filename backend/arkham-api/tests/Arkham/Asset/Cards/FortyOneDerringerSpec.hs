module Arkham.Asset.Cards.FortyOneDerringerSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe ".41 Derringer" $ do
  hasUses @"ammo" Assets.fortyOneDerringer 3
  context "Fight Action" $ do
    it "spend 1 ammo, you get +2 combat, if you succeed by 2 or more it deals +1 damage" . gameTest $ \self -> do
      withProp @"combat" 0 self
      enemy <- testEnemy & prop @"fight" 0 & prop @"health" 3
      location <- testLocation
      fortyOneDerringer <- self `putAssetIntoPlay` Assets.fortyOneDerringer
      self `moveTo` location
      enemy `spawnAt` location
      withEach [(Zero, 2), (MinusOne, 1)] $ \(token, damage) -> do
        setChaosTokens [token]
        [doFight] <- self `getActionsFrom` fortyOneDerringer
        self `useAbility` doFight
        chooseTarget enemy
        startSkillTest
        applyResults
        enemy.damage `shouldReturn` damage
        fortyOneDerringer.ammo `shouldReturn` 2
