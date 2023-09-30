{-# OPTIONS_GHC -Wno-type-defaults #-}

module Arkham.Asset.Cards.Rolands38SpecialSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher (assetIs)
import TestImport.New

default (Int)

spec :: Spec
spec = describe "Roland's .39 Special" $ do
  it "gives +1 combat and +1 damage" . gameTest $ \self -> do
    withProp @"combat" 1 self
    self `putCardIntoPlay` Assets.rolands38Special
    rolands38Special <- selectJust $ assetIs Assets.rolands38Special
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
    self `putCardIntoPlay` Assets.rolands38Special
    rolands38Special <- selectJust $ assetIs Assets.rolands38Special
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
