module Arkham.Treachery.Cards.CaughtInTheCrossfireSpec (spec) where

import Arkham.DamageEffect (nonAttack)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Caught in the Crossfire" do
  it "delays the triggering damage until its nested skill test resolves" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    enemy <- testEnemyWithDef Enemies.gangEnforcer id
    enemy `spawnAt` location
    _ <- self `putTreacheryIntoPlay` Treacheries.caughtInTheCrossfire
    setChaosTokens [AutoFail]

    run $ DealDamage (EnemyTarget enemy.id) (nonAttack (Just self.id) (TestSource mempty) 2)
    useForcedAbility
    chooseSkill SkillIntellect

    enemy.damage `shouldReturn` 0

    startSkillTest
    applyResults
    applyAllDamage

    enemy.damage `shouldReturn` 1
    self.damage `shouldReturn` 1
