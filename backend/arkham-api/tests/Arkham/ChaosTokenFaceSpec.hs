module Arkham.ChaosTokenFaceSpec (spec) where

import Arkham.ChaosToken.Types
import Data.Aeson (decode, encode, toJSON)
import Data.Aeson.Types (Value (String))
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "ChaosTokenFace JSON" do
  it "encodes official faces as legacy constructor-name strings" do
    toJSON Skull `shouldBe` String "Skull"

  it "round-trips official faces" do
    decode (encode ElderSign) `shouldBe` Just ElderSign

  it "encodes custom tokens as their slug" do
    toJSON (CustomToken ":circus-ex-mortis:moon") `shouldBe` String ":circus-ex-mortis:moon"

  it "round-trips custom tokens" do
    decode (encode (CustomToken ":circus-ex-mortis:moon"))
      `shouldBe` Just (CustomToken ":circus-ex-mortis:moon")

  it "falls back to CustomToken for unknown strings" do
    decode "\"some-campaign:comet\"" `shouldBe` Just (CustomToken "some-campaign:comet")

  it "remaps the legacy MoonToken constructor name" do
    decode "\"MoonToken\"" `shouldBe` Just (CustomToken ":circus-ex-mortis:moon")

  it "derives display key, format tag, and label from the slug" do
    customTokenKey ":circus-ex-mortis:moon" `shouldBe` "moon"
    chaosTokenLabel (CustomToken ":circus-ex-mortis:moon") `shouldBe` "Moon"
