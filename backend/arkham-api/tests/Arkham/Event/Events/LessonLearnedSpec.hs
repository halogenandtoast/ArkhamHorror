module Arkham.Event.Events.LessonLearnedSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Phase
import TestImport.New

spec :: Spec
spec = describe "Lesson Learned" do
  -- Survival Knife kills the attacker mid-attack, so by the time the
  -- "after an enemy attacks you" window opens the enemy is in OutOfPlay
  -- RemovedZone. The window used to match nobody and the card was never
  -- offered, while Bounty (an IfEnemyDefeated window) worked fine (#5794).
  it "can be played when the attacking enemy is already dead" . gameTest $ \self -> do
    withProp @"combat" 2 self
    withProp @"resources" 1 self
    location <- testLocation & prop @"clues" 1
    enemy <- testEnemy & prop @"fight" 1 & prop @"health" 1 & prop @"healthDamage" 1
    survivalKnife <- self `putAssetIntoPlay` Assets.survivalKnife
    lessonLearned <- genCard Events.lessonLearned
    setChaosTokens [Zero]

    self `addToHand` lessonLearned
    self `moveTo` location
    enemy `spawnAt` location

    run $ SetPhase EnemyPhase
    enemy `attacks` self

    useReaction
    click "Start skill test"
    click "Apply results"

    assert $ selectNone $ Matcher.EnemyWithId (toId enemy)
    assert survivalKnife.exhausted

    chooseTarget lessonLearned
    self.clues `shouldReturn` 1
    location.clues `shouldReturn` 0
