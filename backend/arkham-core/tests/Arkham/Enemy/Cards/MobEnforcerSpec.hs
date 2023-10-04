module Arkham.Enemy.Cards.MobEnforcerSpec (spec)
where

import Arkham.Ability.Types
import Arkham.Enemy.Cards qualified as Enemies
import TestImport.New

spec :: Spec
spec = describe "Mob Enforcer" $ do
  isHunter Enemies.mobEnforcer

  context "Parley Action" $ do
    it "For an action and 4 resources, can be discarded" . gameTest $ \self -> do
      withProp @"resources" 4 self
      location <- testLocation
      self `moveTo` location
      duringTurn self $ do
        mobEnforcer <- createWeaknessEnemy self Enemies.mobEnforcer
        mobEnforcer `spawnAt` location
        [doParley] <- filter ((== 1) . abilityIndex) <$> self `getActionsFrom` mobEnforcer
        self `useAbility` doParley
        self.engagedEnemies `shouldReturn` []
        self.resources `shouldReturn` 0
        self.remainingActions `shouldReturn` 2
        asDefs self.discard `shouldReturn` [Enemies.mobEnforcer]
