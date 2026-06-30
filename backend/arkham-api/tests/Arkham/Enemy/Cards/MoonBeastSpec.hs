module Arkham.Enemy.Cards.MoonBeastSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Modifier
import Arkham.Projection
import Arkham.Token
import TestImport.New

spec :: Spec
spec = describe "Moon-Beast" do
  -- Moon-Beast has victory points, so on defeat it is added to the victory
  -- display before the IfEnemyDefeated window resolves. Its forced reduce-alarm
  -- ability must use the EnemyDefeated window, which resolves while it is still
  -- in play (regression for issue #4971).
  context "After you defeat Moon-Beast" do
    it "reduces each investigator's alarm level by 1" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      moonBeast <- testEnemyWithDef Enemies.moonBeast id & prop @"fight" 1 & prop @"health" 2
      moonBeast `spawnAt` location
      useForcedAbility -- on-spawn: raise each investigator's alarm level by 1
      run $ PlaceTokens (TestSource mempty) (toTarget self) AlarmLevel 2 -- alarm level now 3
      setChaosTokens [Zero]
      sid <- self `fightEnemy` moonBeast
      run =<< skillTestModifier sid (TestSource mempty) self (DamageDealt 1)
      startSkillTest
      applyResults
      useForcedAbility -- on-defeat: reduce each investigator's alarm level by 1
      fieldMap InvestigatorTokens (countTokens AlarmLevel) (toId self) `shouldReturn` 2
