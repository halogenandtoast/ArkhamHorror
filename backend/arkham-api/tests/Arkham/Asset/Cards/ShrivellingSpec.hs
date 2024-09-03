module Arkham.Asset.Cards.ShrivellingSpec (spec)
where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Shrivelling" $ do
  hasUses @"charge" Assets.shrivelling 4
  context "Fight Action" $ do
    it "uses willpower instead of combat and deals +1 damage" . gameTest $ \self -> do
      withProp @"combat" 0 self
      withProp @"willpower" 1 self
      enemy <- testEnemy & prop @"fight" 1 & prop @"health" 3
      location <- testLocation
      shrivelling <- self `putAssetIntoPlay` Assets.shrivelling
      self `moveTo` location
      enemy `spawnAt` location
      [doFight] <- self `getActionsFrom` shrivelling
      setChaosTokens [Zero]
      self `useAbility` doFight
      chooseTarget enemy
      startSkillTest
      applyResults
      enemy.damage `shouldReturn` 2

    it "deals 1 horror if a skull, cultist, tablet, elder thing, or auto-fail is revealed" . gameTest $ \self -> do
      withProp @"combat" 0 self
      withProp @"willpower" 1 self
      enemy <- testEnemy & prop @"fight" 1 & prop @"health" 3
      location <- testLocation
      shrivelling <- self `putAssetIntoPlay` Assets.shrivelling
      self `moveTo` location
      enemy `spawnAt` location
      [doFight] <- self `getActionsFrom` shrivelling
      withEach [Skull, Cultist, Tablet, ElderThing, AutoFail] $ \token -> do
        setChaosTokens [token]
        self `useAbility` doFight
        chooseTarget enemy
        startSkillTest
        applyAllHorror
        self.horror `shouldReturn` 1
