module Arkham.Asset.Cards.BaseballBatSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Baseball Bat" $ do
  context "Fight action" $ do
    it "gives +2 combat and deals +1 damage" . gameTest $ \self -> do
      withProp @"combat" 0 self
      setChaosTokens [Zero]
      baseballBat <- self `putAssetIntoPlay` Assets.baseballBat
      enemy <- testEnemy & prop @"fight" 2 & prop @"health" 3
      location <- testLocation
      self `moveTo` location
      enemy `spawnAt` location
      [doFight] <- self `getActionsFrom` baseballBat
      self `useAbility` doFight
      chooseTarget enemy
      startSkillTest
      applyResults
      enemy.damage `shouldReturn` 2

    it "is discarded if a Skull or AutoFail is revealed" . gameTest $ \self -> do
      withProp @"combat" 0 self
      baseballBat <- self `putAssetIntoPlay` Assets.baseballBat
      enemy <- testEnemy & prop @"fight" 2 & prop @"health" 3
      location <- testLocation
      self `moveTo` location
      enemy `spawnAt` location
      [doFight] <- self `getActionsFrom` baseballBat

      withEach [Skull, AutoFail] $ \token -> do
        setChaosTokens [token]
        self `useAbility` doFight
        chooseTarget enemy
        startSkillTest
        applyResults
        assert $ selectNone AnyAsset
        asDefs self.discard `shouldMatchListM` [Assets.baseballBat]
