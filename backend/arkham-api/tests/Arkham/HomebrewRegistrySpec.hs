module Arkham.HomebrewRegistrySpec (spec) where

import Arkham.Homebrew.Defs qualified as Defs
import Arkham.Id (unCampaignId)
import Arkham.Homebrew.Registry qualified as Registry
import Arkham.Homebrew.Tokens qualified as Tokens
import Data.Map.Strict qualified as Map
import Test.Hspec
import Prelude

-- Instance discovery is silent-by-construction: if the aggregators or TH
-- fold break, everything compiles and homebrew content just vanishes. These
-- assertions pin the discovered totals for the known campaigns.
spec :: Spec
spec = describe "Homebrew instance discovery" do
  it "discovers both campaigns" do
    map (unCampaignId . fst) Registry.campaigns
      `shouldMatchList` [":dark-matter", ":circus-ex-mortis"]

  it "discovers all sixteen scenarios" do
    length Registry.scenarios `shouldBe` 16
    length Registry.scenarioSets `shouldBe` 16

  it "discovers card definitions from every campaign" do
    Map.size Defs.locationsMap `shouldSatisfy` (> 100)
    Map.size Defs.enemiesMap `shouldSatisfy` (> 50)
    Map.size Defs.storiesMap `shouldSatisfy` (> 40)

  it "discovers entity implementations" do
    length Registry.locations `shouldSatisfy` (> 20)
    length Registry.enemies `shouldSatisfy` (> 20)

  it "discovers custom tokens" do
    Map.keys Tokens.customTokenDefs `shouldBe` [":circus-ex-mortis:moon"]
