module Arkham.Asset.Assets.DirtyFighting2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.TheDunwichLegacy.Whippoorwills qualified as Enemies
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Dirty Fighting (2)" $ do
  context "After you evade an enemy" $ do
    it "can be exhausted to fight an unengaged aloof enemy" . gameTest $ \self -> do
      whippoorwill <- testEnemyWithDef Enemies.whippoorwill id & prop @"evade" 0
      location <- testLocation
      withProp @"agility" 1 self
      setChaosTokens [Zero]
      self `putCardIntoPlay` Assets.dirtyFighting2
      self `moveTo` location
      whippoorwill `spawnAt` location
      self `evadeEnemy` whippoorwill
      startSkillTest
      applyResults
      useReaction
      chooseFight

    it "does not otherwise make an unengaged aloof enemy fightable" . gameTest $ \self -> do
      whippoorwill <- testEnemyWithDef Enemies.whippoorwill id
      location <- testLocation
      self `moveTo` location
      whippoorwill `spawnAt` location
      assertNone $ CanFightEnemy (toSource self)
