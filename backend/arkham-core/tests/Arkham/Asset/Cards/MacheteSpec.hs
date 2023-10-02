module Arkham.Asset.Cards.MacheteSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Machete" $ do
  it "gives +1 combat and +1 damage if the attacked enemy is the only enemy engaged with you" . gameTest $ \self -> do
    withProp @"combat" 1 self
    machete <- self `putAssetIntoPlay` Assets.machete
    enemy <- testEnemy & prop @"fight" 2 & prop @"health" 3
    location <- testLocation
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy `spawnAt` location
    self `moveTo` location
    [doFight] <- machete.abilities
    self `useAbility` doFight
    click "choose enemy"
    click "start skill test"
    click "apply results"
    enemy.damage `shouldReturn` 2

  it "does not give additional damage if the attacked enemy is not engaged with you" . gameTest $ \self -> do
    withProp @"combat" 1 self
    machete <- self `putAssetIntoPlay` Assets.machete
    enemy <- testEnemy & prop @"fight" 2 & prop @"health" 3 & exhausted
    location <- testLocation
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy `spawnAt` location
    self `moveTo` location
    [doFight] <- machete.abilities
    self `useAbility` doFight
    click "choose enemy"
    click "start skill test"
    click "apply results"
    enemy.damage `shouldReturn` 1

  it "does not give additional damage if the attacked enemy is not the only enemy engaged with you" . gameTest $ \self -> do
    withProp @"combat" 1 self
    machete <- self `putAssetIntoPlay` Assets.machete
    enemy1 <- testEnemy & prop @"fight" 2 & prop @"health" 3
    enemy2 <- testEnemy & prop @"fight" 2 & prop @"health" 3
    location <- testLocation
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy1 `spawnAt` location
    enemy2 `spawnAt` location
    self `moveTo` location
    [doFight] <- machete.abilities
    self `useAbility` doFight
    chooseOptionMatching "choose enemy1" $ \case
      FightLabel {enemyId} -> enemyId == toId enemy1
      _ -> False
    click "start skill test"
    click "apply results"
    enemy1.damage `shouldReturn` 1
