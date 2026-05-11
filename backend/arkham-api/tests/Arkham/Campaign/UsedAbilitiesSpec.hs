module Arkham.Campaign.UsedAbilitiesSpec (spec) where

import Arkham.Campaign.Campaigns.NightOfTheZealot
import Arkham.Campaign.Types
import Arkham.Difficulty
import Data.Aeson (Result (..))
import Data.Aeson.KeyMap qualified as KeyMap
import TestImport

-- Stores PerCampaign ability uses on the campaign so they survive ResetGame
-- between scenarios. Existing in-progress campaign saves predate the field,
-- so the parser must default to [] when "usedAbilities" is absent.
spec :: Spec
spec = describe "Campaign usedAbilities" do
  it "round-trips through JSON" do
    let attrs = toAttrs (nightOfTheZealot Easy)
    case eitherDecode' @CampaignAttrs (encode attrs) of
      Left err -> fail $ "Could not roundtrip CampaignAttrs: " <> err
      Right decoded ->
        campaignUsedAbilities decoded `shouldBe` campaignUsedAbilities attrs

  it "writes the field on serialization" do
    let attrs = toAttrs (nightOfTheZealot Easy)
    case toJSON attrs of
      Object o -> KeyMap.member "usedAbilities" o `shouldBe` True
      _ -> fail "expected object"

  it "defaults to [] when the field is absent (legacy saves)" do
    let attrs = toAttrs (nightOfTheZealot Easy)
    let stripped = case toJSON attrs of
          Object o -> Object (KeyMap.delete "usedAbilities" o)
          v -> v
    case fromJSON @CampaignAttrs stripped of
      Error err -> fail $ "Could not parse legacy CampaignAttrs: " <> err
      Success decoded -> campaignUsedAbilities decoded `shouldBe` []
