module Arkham.Asset.Cards.BeckySpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (tommyMuldoon)
import TestImport.New

spec :: Spec
spec = describe "Becky" $ do
  hasUses @"ammo" Assets.becky 2

  it "Each resource gained by tommy muldoon's ability may be placed on Becky as ammo" . gameTestWith tommyMuldoon $ \self -> do
    becky <- self `putAssetIntoPlay` Assets.becky
    beatCop2 <- self `putAssetIntoPlay` Assets.beatCop2
    run $ AssetDamage beatCop2 (TestSource mempty) 3 1
    useReaction
    resolveAmounts self [("Tommy Muldoon Resources", 2), ("Becky Resources", 2)]
    self.resources `shouldReturn` 2
    -- 2 initial + 2 from tommy's ability
    becky.ammo `shouldReturn` 4

  it "gives +2 combat and +1 damage" . gameTestWith tommyMuldoon $ \self -> do
    withProp @"combat" 1 self
    becky <- self `putAssetIntoPlay` Assets.becky
    enemy <- testEnemy & prop @"fight" 3 & prop @"health" 3
    location <- testLocation
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    [doFight] <- self `getActionsFrom` becky
    self `useAbility` doFight
    chooseTarget enemy
    startSkillTest
    applyResults
    enemy.damage `shouldReturn` 2
