module Arkham.UltimatumsAndBoons.UltimatumOfBrokenPromisesSpec (spec) where

import Arkham.ChaosBag.Base
import Arkham.Helpers.Scenario
import Arkham.Scenario.Types (Field (..))
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Ultimatum of Broken Promises" $ do
  it "removes the elder sign token when the chaos bag is built" . gameTest $ \_ -> do
    withUltimatums [UltimatumOfBrokenPromises]
    setChaosTokens [ElderSign, Zero]
    tokens <- scenarioFieldMap ScenarioChaosBag (map chaosTokenFace . chaosBagChaosTokens)
    tokens `shouldMatchList` [Zero]

  it "has no effect while ultimatums and boons are disabled" . gameTest $ \_ -> do
    withUltimatumsDisabled [UltimatumOfBrokenPromises]
    setChaosTokens [ElderSign, Zero]
    tokens <- scenarioFieldMap ScenarioChaosBag (map chaosTokenFace . chaosBagChaosTokens)
    tokens `shouldMatchList` [ElderSign, Zero]
