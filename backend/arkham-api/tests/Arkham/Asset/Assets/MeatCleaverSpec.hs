module Arkham.Asset.Assets.MeatCleaverSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Meat Cleaver" do
  -- Regression guard for #5530. The effect recognises its own killing blow by
  -- comparing the Defeated source against its ability source, and attack damage
  -- now carries the attacker (UseAbilitySource) instead of a bare AbilitySource.
  it "offers to heal 1 horror when the attack defeats an enemy" . gameTest $ \self -> do
    withProp @"combat" 4 self
    meatCleaver <- self `putAssetIntoPlay` Assets.meatCleaver
    enemy <- testEnemy & prop @"fight" 1 & prop @"health" 2
    location <- testLocation
    setChaosTokens [Zero]
    run $ placedLocation location
    enemy `spawnAt` location
    self `moveTo` location

    [doFight] <- meatCleaver.abilities
    self `useAbility` doFight
    payUpTo 1 1
    applyAllHorror
    self.horror `shouldReturn` 1
    click "choose enemy"
    click "start skill test"
    click "apply results"

    chooseOptionMatching "heal 1 horror" \case
      HorrorLabel iid _ -> iid == self.id
      _ -> False
    self.horror `shouldReturn` 0
