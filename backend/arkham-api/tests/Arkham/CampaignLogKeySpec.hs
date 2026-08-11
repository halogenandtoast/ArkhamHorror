module Arkham.CampaignLogKeySpec (spec) where

import Arkham.CampaignLogKey
import Data.Aeson (decode, encode)
import Data.Text (pack, unpack)
import Test.Hspec
import Text.Read (readMaybe)
import Prelude

-- Stand-in for a homebrew campaign's own key enum, so this spec tests the
-- shared wrapper rather than whichever campaigns happen to exist.
data TestKey = Memories
  deriving stock (Show, Read, Eq)

instance IsCampaignLogKey TestKey where
  toCampaignLogKey = HomebrewCampaignLogKey . pack . show
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t -> readMaybe (unpack t)
    _ -> Nothing

spec :: Spec
spec = describe "CampaignLogKey JSON" do
  it "round-trips core keys" do
    decode (encode DrivenInsaneInvestigators) `shouldBe` Just DrivenInsaneInvestigators

  it "round-trips a homebrew campaign key through the shared wrapper" do
    let k = toCampaignLogKey Memories
    decode (encode k) `shouldBe` Just k

  it "recovers the owning campaign's key from the log key" do
    fromCampaignLogKey (toCampaignLogKey Memories) `shouldBe` Just Memories

  it "does not recover a homebrew key from a core log key" do
    fromCampaignLogKey DrivenInsaneInvestigators `shouldBe` (Nothing :: Maybe TestKey)
