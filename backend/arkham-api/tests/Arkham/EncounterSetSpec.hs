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
    toJSON (Homebrew "z-dark-matter:anachronism") `shouldBe` String "z-dark-matter:anachronism"

  it "round-trips homebrew sets" do
    decode (encode (Homebrew "z-dark-matter:anachronism"))
      `shouldBe` Just (Homebrew "z-dark-matter:anachronism")

  it "falls back to Homebrew for unknown strings" do
    decode "\"some-future-set\"" `shouldBe` Just (Homebrew "some-future-set")

  it "remaps legacy homebrew constructor names to slugs" do
    decode "\"DarkMatterAnachronism\"" `shouldBe` Just (Homebrew "z-dark-matter:anachronism")
    decode "\"CircusExMortisOneNightOnly\"" `shouldBe` Just (Homebrew "z-circus-ex-mortis:one_night_only")
