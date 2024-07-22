module Arkham.Event.Cards.MindOverMatterSpec (spec) where

import Arkham.Calculation
import Arkham.Event.Cards qualified as Events
import Arkham.Scenarios.UnionAndDisillusion.Helpers
import TestImport.New

spec :: Spec
spec = describe "Mind over Matter" $ do
  it "allows you to use intellect in place of your combat and agility" . gameTest $ \self -> do
    withProp @"intellect" 1 self
    withProp @"combat" 0 self
    withProp @"agility" 0 self
    setChaosTokens [Zero]
    duringTurn self $ do
      self `playEvent` Events.mindOverMatter
      sid1 <- getRandom
      run $ beginSkillTest sid1 self #combat 1
      chooseOptionMatching "use intellect instead of combat" \case
        SkillLabel SkillIntellect _ -> True
        _ -> False
      startSkillTest
      assertPassedSkillTest
      applyResults

      sid2 <- getRandom
      run $ beginSkillTest sid2 self #agility 1
      chooseOptionMatching "use intellect instead of agility" \case
        SkillLabel SkillIntellect _ -> True
        _ -> False
      startSkillTest
      assertPassedSkillTest

  context "circle tests" $ do
    it "should replace the relevant type" . gameTest $ \self -> do
      withProp @"intellect" 1 self
      withProp @"combat" 0 self
      withProp @"agility" 0 self
      setChaosTokens [Zero]
      duringTurn self $ do
        self `playEvent` Events.mindOverMatter
        sid <- getRandom
        circleTest
          sid
          (toId self)
          (TestSource mempty)
          TestTarget
          [#intellect, #combat]
          (Fixed 2)
        runMessages

        chooseOptionMatching "use intellect instead of combat" \case
          SkillLabel SkillIntellect _ -> True
          _ -> False
        startSkillTest
        assertPassedSkillTest
        applyResults

    it "should replace multiple" . gameTest $ \self -> do
      withProp @"intellect" 1 self
      withProp @"combat" 0 self
      withProp @"agility" 0 self
      withProp @"willpower" 0 self
      setChaosTokens [Zero]
      duringTurn self $ do
        self `playEvent` Events.mindOverMatter
        sid <- getRandom
        circleTest
          sid
          (toId self)
          (TestSource mempty)
          TestTarget
          [#intellect, #combat, #agility, #willpower]
          (Fixed 3)
        runMessages

        chooseOptionMatching "use intellect instead of combat" \case
          SkillLabel SkillIntellect _ -> True
          _ -> False
        chooseOptionMatching "use intellect instead of agility" \case
          SkillLabel SkillIntellect _ -> True
          _ -> False
        startSkillTest
        assertPassedSkillTest
        applyResults
