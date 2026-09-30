module Arkham.Helpers.WindowSpec (spec) where

import Arkham.Attack qualified as Attack
import Arkham.Helpers.Window (windowMatches)
import Arkham.Matcher qualified as Matcher
import Arkham.Placement
import Arkham.Window qualified as Window
import Arkham.Zone
import TestImport.New

-- An attack still resolves when its attacker is defeated part-way through
-- (Survival Knife's enemy-phase fight, Counterattack), so the After windows do
-- open -- but the enemy is sitting in OutOfPlay RemovedZone by then, and bare
-- enemy queries skip out-of-play zones. These matchers therefore have to go
-- through enemyMatches, or every reaction on a surviving source silently
-- vanishes: Lesson Learned, Service Revolver, Bangle of Jinxes (1), Horacio
-- Martinez and Daniela Reyes (2)'s own free reaction (#5794).
--
-- The same holds for the windows on your own attacks, where killing the target
-- is the normal outcome rather than an edge case.
spec :: Spec
spec = describe "after-attack windows with a removed enemy" do
  it "EnemyAttacks matches an attacker defeated during its own attack" . gameTest $ \self -> do
    location <- testLocation
    enemy <- testEnemy
    self `moveTo` location
    enemy `spawnAt` location
    let details = Attack.enemyAttack (toId enemy) (toId enemy) (toId self)
    run $ PlaceEnemy (toId enemy) (OutOfPlay RemovedZone)
    assertNone Matcher.AnyEnemy
    windowMatches
      (toId self)
      (TestSource mempty)
      (Window.mkAfter $ Window.EnemyAttacks details)
      (Matcher.EnemyAttacks #after Matcher.You Matcher.AnyEnemyAttack Matcher.AnyEnemy)
      `shouldReturn` True

  it "EnemyAttacksEvenIfCancelled matches an attacker defeated during its own attack" . gameTest $ \self -> do
    location <- testLocation
    enemy <- testEnemy
    self `moveTo` location
    enemy `spawnAt` location
    let details = Attack.enemyAttack (toId enemy) (toId enemy) (toId self)
    run $ PlaceEnemy (toId enemy) (OutOfPlay RemovedZone)
    windowMatches
      (toId self)
      (TestSource mempty)
      (Window.mkAfter $ Window.EnemyAttacksEvenIfCancelled details)
      (Matcher.EnemyAttacksEvenIfCancelled #after Matcher.You Matcher.AnyEnemyAttack Matcher.AnyEnemy)
      `shouldReturn` True

  it "EnemyAttacked matches an enemy your attack just defeated" . gameTest $ \self -> do
    location <- testLocation
    enemy <- testEnemy
    self `moveTo` location
    enemy `spawnAt` location
    run $ PlaceEnemy (toId enemy) (OutOfPlay RemovedZone)
    windowMatches
      (toId self)
      (TestSource mempty)
      (Window.mkAfter $ Window.EnemyAttacked (toId self) (TestSource mempty) (toId enemy))
      (Matcher.EnemyAttacked #after Matcher.You Matcher.AnySource Matcher.AnyEnemy)
      `shouldReturn` True

  -- NonEliteEnemy (Grievous Wound, Horacio Martinez) goes past the AnyEnemy
  -- short-circuit and has to resolve its submatcher against the removed entity.
  it "EnemyAttackedSuccessfully resolves a submatcher against the removed enemy" . gameTest $ \self -> do
    location <- testLocation
    enemy <- testEnemy
    self `moveTo` location
    enemy `spawnAt` location
    run $ PlaceEnemy (toId enemy) (OutOfPlay RemovedZone)
    windowMatches
      (toId self)
      (TestSource mempty)
      (Window.mkAfter $ Window.SuccessfulAttackEnemy (toId self) (TestSource mempty) (toId enemy) 1)
      (Matcher.EnemyAttackedSuccessfully #after Matcher.You Matcher.AnySource Matcher.NonEliteEnemy)
      `shouldReturn` True

  it "does not match a submatcher the removed enemy fails" . gameTest $ \self -> do
    location <- testLocation
    enemy <- testEnemy & elite
    self `moveTo` location
    enemy `spawnAt` location
    run $ PlaceEnemy (toId enemy) (OutOfPlay RemovedZone)
    windowMatches
      (toId self)
      (TestSource mempty)
      (Window.mkAfter $ Window.SuccessfulAttackEnemy (toId self) (TestSource mempty) (toId enemy) 1)
      (Matcher.EnemyAttackedSuccessfully #after Matcher.You Matcher.AnySource Matcher.NonEliteEnemy)
      `shouldReturn` False
