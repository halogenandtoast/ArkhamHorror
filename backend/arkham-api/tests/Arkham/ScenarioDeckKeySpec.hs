module Arkham.ScenarioDeckKeySpec (spec) where

import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern ScanningDeck)
import Arkham.Scenario.Deck
import Data.Aeson (decode, encode, toJSON)
import Data.Aeson.Types (Value (String))
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "ScenarioDeckKey JSON" do
  it "encodes core keys as their constructor name" do
    toJSON CultistDeck `shouldBe` String "CultistDeck"

  it "round-trips core keys" do
    decode (encode CultistDeck) `shouldBe` Just CultistDeck

  it "encodes homebrew keys as their bare tag (save-compatible)" do
    toJSON ScanningDeck `shouldBe` String "ScanningDeck"

  it "round-trips homebrew keys" do
    decode (encode ScanningDeck) `shouldBe` Just ScanningDeck

  it "is promotion-safe: a promoted name decodes to the core constructor" do
    decode "\"CultistDeck\"" `shouldBe` Just CultistDeck
