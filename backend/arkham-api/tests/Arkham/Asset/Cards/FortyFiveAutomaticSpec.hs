module Arkham.Asset.Cards.FortyFiveAutomaticSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe ".45 Automatic" $ do
  hasUses @"ammo" Assets.fortyFiveAutomatic 4

  it "gives +1 combat and +1 damage" . gameTest $ \self -> do
    withProp @"combat" 1 self
    fortyFiveAutomatic <- self `putAssetIntoPlay` Assets.fortyFiveAutomatic
    enemy <- testEnemy & prop @"fight" 2 & prop @"health" 3
    location <- testLocation
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    [doFight] <- self `getActionsFrom` fortyFiveAutomatic
    self `useAbility` doFight
    chooseTarget enemy
    startSkillTest
    applyResults
    enemy.damage `shouldReturn` 2
