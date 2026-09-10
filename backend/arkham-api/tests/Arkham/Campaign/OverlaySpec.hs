module Arkham.Campaign.OverlaySpec (spec) where

import Arkham.Campaign.Overlay
import Arkham.Campaign.Types
import Arkham.CampaignStep
import Arkham.Difficulty
import Arkham.Homebrew.CircusExMortis.Campaign (circusExMortis)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import TestImport

spec :: Spec
spec = describe "Campaign overlays" do
  let harmsWay = ScenarioStep ":circus-ex-mortis:040"
      allPointsWest = ScenarioStep ":circus-ex-mortis:074"
      withSteps steps = overAttrs (\attrs -> attrs {campaignCompletedSteps = steps}) (circusExMortis Standard)
      overlay steps = case campaignOverlays $ withSteps steps of
        [o] -> o
        _ -> error "expected one Circus Ex Mortis overlay"

  it "does not activate the Rougarou overlay before Harm's Way" do
    (overlay []).active `shouldBe` False
    (overlay []).available `shouldBe` False
    campaignAbilities (withSteps []) `shouldBe` []

  it "offers the free side story immediately after Harm's Way" do
    let o = overlay [harmsWay]
    o.active `shouldBe` True
    o.available `shouldBe` True
    o.xpCost `shouldBe` 0
    o.scenario `shouldBe` "81001"
    length (campaignAbilities $ withSteps [harmsWay]) `shouldBe` 2

  it "keeps card replacements after the discount window has closed" do
    let o = overlay [allPointsWest, harmsWay]
    o.active `shouldBe` True
    o.available `shouldBe` False
    Map.lookup "81019" o.cardReplacements `shouldBe` Just ":circus-ex-mortis:019c"
    Map.lookup "81029" o.cardReplacements `shouldBe` Just ":circus-ex-mortis:029c"

  it "exposes overlays for existing campaign saves without a migration" do
    let gameCampaign = withSteps [harmsWay]
    case toJSON (Campaign gameCampaign) of
      Object o -> KeyMap.lookup "overlays" o `shouldBe` Just (toJSON $ campaignOverlays gameCampaign)
      _ -> fail "expected a campaign object"
