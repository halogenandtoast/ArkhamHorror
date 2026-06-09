module Arkham.Skill.Cards.WatchThisSpec (spec) where

import Arkham.Skill.Cards qualified as Cards
import TestImport.New

spec :: Spec
spec = describe "Watch This" $ do
  -- Regression for #4760: when Watch This and Out the Door are committed to the
  -- same skill test, Watch This' additional "spend up to 3 resources" cost must
  -- be paid before Out the Door's "+2 resources when committed" resolves. Out
  -- the Door must not inflate the amount you are allowed to gamble.
  it "pays its additional cost before Out the Door grants resources (#4760)" . gameTest $ \self -> do
    withProp @"agility" 2 self
    withProp @"resources" 1 self
    watchThis <- genCard Cards.watchThis
    outTheDoor <- genCard Cards.outTheDoor
    self `addToHand` watchThis
    self `addToHand` outTheDoor
    setChaosTokens [Zero]
    sid <- getRandom
    run $ beginSkillTest sid self #agility 1
    commit watchThis
    commit outTheDoor
    startSkillTest
    -- Only the 1 starting resource is spendable here; Out the Door's +2 has not
    -- resolved yet. Before the fix this offered up to 3.
    payUpTo 1 1
    applyResults
    -- 1 - 1 (paid) + 2 (Out the Door) + 2 (Watch This: 2x resources paid) = 4
    self.resources `shouldReturn` 4
