module Arkham.ActionSpec (spec) where

import Arkham.Action
import Arkham.Homebrew.DarkMatter.Actions (pattern Scan)
import Arkham.Homebrew.Defs (allActions)
import Data.Aeson (decode, encode, toJSON)
import Data.Aeson.Types (Value (String))
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "Action JSON" do
  it "encodes core actions as their constructor name" do
    toJSON Fight `shouldBe` String "Fight"

  it "round-trips core actions" do
    decode (encode Fight) `shouldBe` Just Fight

  it "encodes homebrew actions as their bare tag" do
    toJSON Scan `shouldBe` String "Scan"

  it "round-trips homebrew actions" do
    decode (encode Scan) `shouldBe` Just Scan

  it "is promotion-safe: a homebrew tag matches the core encoding of that name" do
    toJSON (HomebrewAction "Fight") `shouldBe` toJSON Fight

  it "is promotion-safe: a promoted name decodes to the core constructor" do
    decode "\"Fight\"" `shouldBe` Just Fight

  it "folds every campaign's actions into the global universe" do
    Scan `shouldSatisfy` (`elem` allActions)
    Fight `shouldSatisfy` (`elem` allActions)
