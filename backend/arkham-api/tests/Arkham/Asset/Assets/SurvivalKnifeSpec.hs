module Arkham.Asset.Assets.SurvivalKnifeSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Phase
import TestImport.New

spec :: Spec
spec = describe "Survival Knife" do
  it "fights the attacking enemy when all attack damage is assigned to an asset" . gameTest $ \self -> do
    withProp @"combat" 2 self
    location <- testLocation
    enemy <- testEnemy & prop @"fight" 3 & prop @"health" 3
    beatCop <- self `putAssetIntoPlay` Assets.beatCop
    survivalKnife <- self `putAssetIntoPlay` Assets.survivalKnife
    setChaosTokens [Zero]

    self `moveTo` location
    enemy `spawnAt` location

    run $ SetPhase EnemyPhase
    run $ InvestigatorAssignDamage self.id (EnemyAttackSource $ toId enemy) DamageAny 1 0
    chooseOptionMatching "assign damage to asset" \case
      AssetDamageLabel aid _ -> aid == beatCop
      _ -> False
    applyAllDamage

    useReaction
    chooseOnlyOption "Start skill test"
    chooseOnlyOption "Apply Results"

    enemy.damage `shouldReturn` 2
    assert survivalKnife.exhausted
