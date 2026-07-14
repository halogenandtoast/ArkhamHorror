module Arkham.EncounterSetSpec (spec) where

import Arkham.EncounterSet
import Data.Aeson (decode, encode, toJSON)
import Data.Aeson.Types (Value (String))
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "EncounterSet JSON" do
  it "encodes official sets as legacy constructor-name strings" do
    toJSON TheGathering `shouldBe` String "TheGathering"

  it "round-trips official sets" do
    decode (encode TheGathering) `shouldBe` Just TheGathering

  it "encodes homebrew sets as their slug" do
    toJSON (Homebrew ":dark-matter:anachronism") `shouldBe` String ":dark-matter:anachronism"

  it "round-trips homebrew sets" do
    decode (encode (Homebrew ":dark-matter:anachronism"))
      `shouldBe` Just (Homebrew ":dark-matter:anachronism")

  it "falls back to Homebrew for unknown strings" do
    decode "\"some-future-set\"" `shouldBe` Just (Homebrew "some-future-set")
