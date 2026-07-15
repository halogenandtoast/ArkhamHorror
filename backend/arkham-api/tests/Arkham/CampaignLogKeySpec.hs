module Arkham.CampaignLogKeySpec (spec) where

import Arkham.CampaignLogKey
import Arkham.Homebrew.DarkMatter.Key (DarkMatterKey (Memories))
import Data.Aeson (decode, encode)
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "CampaignLogKey JSON" do
  it "round-trips core keys" do
    decode (encode DrivenInsaneInvestigators) `shouldBe` Just DrivenInsaneInvestigators

  it "round-trips a homebrew campaign key through the shared wrapper" do
    let k = toCampaignLogKey Memories
    decode (encode k) `shouldBe` Just k

  it "recovers the owning campaign's key from the log key" do
    fromCampaignLogKey (toCampaignLogKey Memories) `shouldBe` Just Memories
