module Arkham.Asset.Cards.KnifeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Knife" $ do
  it "Fight. You get +1 for this attack." . gameTest $ \self -> do
    withProp @"combat" 2 self
    knife <- self `putAssetIntoPlay` Assets.knife
    enemy <- testEnemy & prop @"health" 3 & prop @"fight" 3
    location <- testLocation
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    [knifeFightAction, _] <- self `getActionsFrom` knife
    self `useAbility` knifeFightAction
    chooseTarget enemy
    chooseOnlyOption "Start skill test"
    chooseOnlyOption "Apply Results"
    enemy.damage `shouldReturn` 1
    self.discard `shouldSatisfyM` null

  it "Discard Knife: Fight. You get +2 for this attack. This attack deals +1 damage." . gameTest $ \self -> do
    withProp @"combat" 1 self
    knife <- self `putAssetIntoPlay` Assets.knife
    enemy <- testEnemy & prop @"health" 3 & prop @"fight" 3
    location <- testLocation
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    [_, knifeDiscardFightAction] <- self `getActionsFrom` knife
    self `useAbility` knifeDiscardFightAction
    chooseTarget enemy
    chooseOnlyOption "Start skill test"
    chooseOnlyOption "Apply Results"
    enemy.damage `shouldReturn` 2
    asDefs self.discard `shouldReturn` [Assets.knife]
