module Arkham.Scenario.Scenarios.TheThingInTheDepthsSpec (spec) where

import Arkham.Matcher
import Arkham.Modifier
import TestImport.New

-- The Day 2 intro ("We can push!" / "We'll rev the engine!") applies its penalty
-- to the /next/ scenario, not the current one. It is carried via
-- 'nextSetupModifier' (an 'EffectNextSetupWindow' tagged with the scenario the
-- choice was made in): inert while that scenario is current, active once a
-- different scenario is.
spec :: Spec
spec = describe "The Thing in the Depths" do
  describe "next-scenario setup penalties" do
    it "does not apply the penalty during the scenario the choice was made in" . gameTest $ \self -> do
      sid <- selectJust TheScenario
      run =<< nextSetupModifier sid ScenarioSource (toId self) (StartingResources (-2))
      getModifiers self `shouldNotContainM` [StartingResources (-2)]

    it "applies the resource penalty once a different scenario is current" . gameTest $ \self -> do
      run =<< nextSetupModifier (ScenarioId "00000") ScenarioSource (toId self) (StartingResources (-2))
      getModifiers self `shouldContainM` [StartingResources (-2)]

    it "carries the action penalty to the next scenario too" . gameTest $ \self -> do
      run =<< nextSetupModifier (ScenarioId "00000") ScenarioSource (toId self) (FewerActions 1)
      getModifiers self `shouldContainM` [FewerActions 1]
