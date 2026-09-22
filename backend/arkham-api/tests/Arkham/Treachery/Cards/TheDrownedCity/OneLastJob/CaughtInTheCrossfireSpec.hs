module Arkham.Treachery.Cards.TheDrownedCity.OneLastJob.CaughtInTheCrossfireSpec (spec) where

import Arkham.DamageEffect (delayDamage, nonAttack)
import Arkham.Enemy.CardDefs.TheDrownedCity.OneLastJob qualified as Enemies
import Arkham.Matcher
import Arkham.Treachery.CardDefs.TheDrownedCity.OneLastJob qualified as Treacheries
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

  it "initiates once per enemy when the damage is dealt simultaneously" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    enemy1 <- testEnemyWithDef Enemies.gangEnforcer id
    enemy2 <- testEnemyWithDef Enemies.gangInformant id
    enemy1 `spawnAt` location
    enemy2 `spawnAt` location
    _ <- self `putTreacheryIntoPlay` Treacheries.caughtInTheCrossfire
    setChaosTokens [AutoFail]

    -- Damage both enemies simultaneously from inside an open skill test window, the
    -- shape Storm of Spirits produces: delayed damage with the defeat checks queued by
    -- the source behind the batch, and the two DealtDamage windows merged into one
    -- check so the Forced ability is offered once carrying both initiations.
    sid <- getRandom
    runSkillTest sid self #intellect 2
    pushAll
      [ Simultaneously
          [ DealDamage (EnemyTarget enemy1.id) (delayDamage $ nonAttack (Just self.id) (TestSource mempty) 3)
          , DealDamage (EnemyTarget enemy2.id) (delayDamage $ nonAttack (Just self.id) (TestSource mempty) 3)
          ]
      , Simultaneously
          [ checkDefeated (TestSource mempty) enemy1.id
          , checkDefeated (TestSource mempty) enemy2.id
          ]
      ]
    applyResults

    -- one initiation per enemy behind a single button: triggering it asks which enemy
    -- this use resolves against while several remain, then goes direct once one is left
    useForcedAbility
    chooseTarget enemy1
    chooseSkill SkillIntellect

    -- nothing has applied yet: enemy1's damage rides behind its test, enemy2's is held
    -- by the initiation queue
    enemy1.damage `shouldReturn` 0
    enemy2.damage `shouldReturn` 0

    startSkillTest
    applyResults
    applyAllDamage

    -- the first initiation resolves IN FULL -- test, reduction, damage -- before the
    -- second is even offered
    enemy1.damage `shouldReturn` 2

    useForcedAbility
    chooseSkill SkillIntellect
    startSkillTest
    applyResults
    applyAllDamage

    -- 3 damage reduced to 2: the enforcer (3 health) survives, the informant (1) is
    -- defeated by the check that came back with its held damage
    enemy1.damage `shouldReturn` 2
    assert $ selectNone $ EnemyWithId enemy2.id
    self.damage `shouldReturn` 2
