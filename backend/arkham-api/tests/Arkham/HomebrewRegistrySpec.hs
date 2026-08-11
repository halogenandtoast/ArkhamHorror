module Arkham.HomebrewRegistrySpec (spec) where

import Arkham.Homebrew.Registry qualified as Registry
import Test.Hspec
import Prelude

-- Instance discovery is silent-by-construction: if the aggregators or TH fold
-- break, everything compiles and homebrew content just vanishes. These
-- assertions are deliberately content-independent — adding or removing a
-- campaign must not touch this file.
spec :: Spec
spec = describe "Homebrew instance discovery" do
  it "discovers campaigns, scenarios, and cards at all" do
    -- length, not the lists themselves: these registry types have no Show
    length Registry.campaigns `shouldSatisfy` (> 0)
    length Registry.scenarios `shouldSatisfy` (> 0)
    length Registry.locations `shouldSatisfy` (> 0)
    length Registry.enemies `shouldSatisfy` (> 0)

  it "pairs every discovered scenario with its encounter set" do
    map fst Registry.scenarios `shouldMatchList` map fst Registry.scenarioSets
