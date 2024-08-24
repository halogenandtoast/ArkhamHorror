module Arkham.Enemy.Cards.WatcherFromAnotherDimensionSpec (spec) where

import Arkham.Action (Action)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Cards (patriceHathaway)
import Arkham.Matcher
import Arkham.Placement
import TestImport.New

{-

Forced - When your deck runs out of cards, if this enemy is in your hand: It attacks you (from your hand).
-}

spec :: Spec
spec = describe "Watcher from Another Dimension" do
  context "Revelation" do
    it "Secretly add this enemy to your hand" . gameTestWith patriceHathaway $ \self -> do
      flashlight <- genPlayerCard Assets.flashlight
      withProp @"deck" (Deck [flashlight]) self
      self `drawsCard` Enemies.watcherFromAnotherDimension
      assertAny
        $ EnemyWithPlacement (StillInHand $ toId self)
        <> enemyIs Enemies.watcherFromAnotherDimension

  for_ ([#fight, #evade] :: [Action]) \action -> do
    context ("You may " <> show action <> " this enemy while it is in your hand") do
      it "if you succeed discard it from your hand" . gameTestWith patriceHathaway $ \self -> do
        flashlight <- genPlayerCard Assets.flashlight
        withProp @"deck" (Deck [flashlight]) self
        withProp @"combat" 5 self
        withProp @"agility" 5 self
        setChaosTokens [Zero]
        self `drawsCard` Enemies.watcherFromAnotherDimension
        watcher <- selectJust $ enemyIs Enemies.watcherFromAnotherDimension
        [fightAction, evadeAction] <- self `getActionsFrom` watcher
        if action == #fight
          then self `useAbility` fightAction
          else self `useAbility` evadeAction
        startSkillTest
        applyResults
        assertNone $ enemyIs Enemies.watcherFromAnotherDimension
        asDefs self.discard `shouldMatchListM` [Enemies.watcherFromAnotherDimension]

      it "If you fail, spawn it engaged with you" . gameTestWith patriceHathaway $ \self -> do
        location <- testLocation
        self `moveTo` location
        flashlight <- genPlayerCard Assets.flashlight
        withProp @"deck" (Deck [flashlight]) self
        withProp @"combat" 0 self
        withProp @"agility" 0 self
        setChaosTokens [Zero]
        self `drawsCard` Enemies.watcherFromAnotherDimension
        watcher <- selectJust $ enemyIs Enemies.watcherFromAnotherDimension
        [fightAction, evadeAction] <- self `getActionsFrom` watcher
        if action == #fight
          then self `useAbility` fightAction
          else self `useAbility` evadeAction
        startSkillTest
        applyResults
        assertAny
          $ enemyIs Enemies.watcherFromAnotherDimension
          <> EnemyWithPlacement (InThreatArea $ toId self)
        asDefs self.discard `shouldMatchListM` []

  context "When your deck runs out of cards" do
    it "if this enemy is in your hand: It attacks you (from your hand)" . gameTestWith patriceHathaway $ \self -> do
      flashlight <- genPlayerCard Assets.flashlight
      knives <- replicateM 10 $ genPlayerCard Assets.knife
      withProp @"deck" (Deck [flashlight]) self
      withProp @"discard" knives self
      self `drawsCard` Enemies.watcherFromAnotherDimension
      self `drawCards` 1
      useForcedAbility
      applyAllDamage
      self.damage `shouldReturn` 3
