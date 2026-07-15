module Arkham.TraitSpec (spec) where

import Arkham.Homebrew.Defs (allTraits)
import Arkham.Trait
import Data.Aeson (decode, encode, toJSON)
import Data.Aeson.Types (Value (String))
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "Trait JSON" do
  it "encodes core traits as their constructor name" do
    toJSON Ally `shouldBe` String "Ally"

  it "round-trips core traits" do
    decode (encode Ally) `shouldBe` Just Ally

  it "encodes homebrew traits as their bare tag" do
    toJSON (HomebrewTrait "Moon") `shouldBe` String "Moon"

  it "round-trips homebrew traits" do
    decode (encode (HomebrewTrait "Moon")) `shouldBe` Just (HomebrewTrait "Moon")

  it "decodes unknown strings as homebrew traits" do
    decode "\"Moon\"" `shouldBe` Just (HomebrewTrait "Moon")

  -- Promotion safety: a homebrew trait's tag equals the constructor name, so it
  -- serializes identically to a core trait of that name, and FromJSON resolves
  -- core names first. Therefore if a homebrew trait is later promoted into the
  -- core enum, existing serialized data keeps decoding — straight to the real
  -- constructor, with no migration. We demonstrate with "Ally" (already core):
  it "is promotion-safe: a homebrew tag matches the core encoding of that name" do
    toJSON (HomebrewTrait "Ally") `shouldBe` toJSON Ally

  it "is promotion-safe: a promoted name decodes to the core constructor, not HomebrewTrait" do
    decode "\"Ally\"" `shouldBe` Just Ally

  it "folds every campaign's traits into the global universe" do
    -- disjoint: 27 (Dark Matter) + 9 (Circus Ex Mortis)
    length allTraits `shouldBe` length coreTraits + 36
    HomebrewTrait "Moon" `shouldSatisfy` (`elem` allTraits)
    HomebrewTrait "LiberPater" `shouldSatisfy` (`elem` allTraits)
