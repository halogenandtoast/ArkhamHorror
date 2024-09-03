module Arkham.Asset.Cards.Rolands38SpecialSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Roland's .39 Special" $ do
  hasUses @"ammo" Assets.rolands38Special 4

  it "gives +1 combat and +1 damage" . gameTest $ \self -> do
    withProp @"combat" 1 self
    rolands38Special <- self `putAssetIntoPlay` Assets.rolands38Special
    enemy <- testEnemy & prop @"fight" 2 & prop @"health" 3
    location <- testLocation
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy `spawnAt` location
    self `moveTo` location
    [doFight] <- rolands38Special.abilities
    self `useAbility` doFight
    click "choose enemy"
    click "start skill test"
    click "apply results"
    enemy.damage `shouldReturn` 2

  it "gives +3 combat and +1 damage if there are 1 or more clues on your location" . gameTest $ \self -> do
    withProp @"combat" 1 self
    rolands38Special <- self `putAssetIntoPlay` Assets.rolands38Special
    enemy <- testEnemy & prop @"fight" 4 & prop @"health" 3
    location <- testLocation & prop @"clues" 1
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy `spawnAt` location
    self `moveTo` location
    [doFight] <- rolands38Special.abilities
    self `useAbility` doFight
    click "choose enemy"
    click "start skill test"
    click "apply results"
    enemy.damage `shouldReturn` 2
