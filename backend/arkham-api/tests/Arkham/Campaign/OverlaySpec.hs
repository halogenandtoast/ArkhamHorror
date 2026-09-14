module Arkham.Campaign.OverlaySpec (spec) where

import Arkham.Campaign.Overlay
import Arkham.Campaign.Types
import Arkham.CampaignStep
import Arkham.Difficulty
import Arkham.Homebrew.CircusExMortis.Campaign (circusExMortis)
import Data.Aeson.KeyMap qualified as KeyMap
import TestImport

spec :: Spec
spec = describe "Campaign overlays" do
  let harmsWay = ScenarioStep ":circus-ex-mortis:040"
      allPointsWest = ScenarioStep ":circus-ex-mortis:074"
      -- What the continue screen prepends on every answer; it carries no
      -- scenario, so the discount window must look straight past it.
      continuing = ContinueCampaignStep (Continuation harmsWay True False Nothing True)
      withSteps steps = overAttrs (\attrs -> attrs {campaignCompletedSteps = steps}) (circusExMortis Standard)
      overlay steps = case campaignOverlays $ withSteps steps of
        [o] -> o
        _ -> error "expected one Circus Ex Mortis overlay"

  it "does not discount the Rougarou side story before Harm's Way" do
    (overlay []).available `shouldBe` False
    (overlay []).xpCost `shouldBe` 1

  it "offers the free side story immediately after Harm's Way" do
    let o = overlay [continuing, continuing, harmsWay]
    o.available `shouldBe` True
    o.xpCost `shouldBe` 0
    o.scenario `shouldBe` "81001"

  it "closes the discount window once another scenario has been played" do
    let o = overlay [continuing, allPointsWest, continuing, harmsWay]
    o.available `shouldBe` False
    o.xpCost `shouldBe` 1

  it "exposes overlays for existing campaign saves without a migration" do
    let gameCampaign = withSteps [harmsWay]
    case toJSON (Campaign gameCampaign) of
      Object o -> KeyMap.lookup "overlays" o `shouldBe` Just (toJSON $ campaignOverlays gameCampaign)
      _ -> fail "expected a campaign object"
