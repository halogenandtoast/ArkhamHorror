{-# LANGUAGE OverloadedStrings #-}

module Arkham.MessageSpec (spec) where

import TestImport

spec :: Spec
spec = describe "Message JSON compatibility" do
  it "decodes legacy InitDeck messages without decklist context" do
    let iid = "01001"
        murl = Just "https://arkhamdb.com/decklist/view/1" :: Maybe Text
        deck = Deck [] :: Deck PlayerCard
        legacyJson =
          object
            [ "tag" .= ("InitDeck" :: Text)
            , "contents" .= (iid :: InvestigatorId, murl, deck)
            ]

    decode (encode legacyJson) `shouldBe` Just (InitDeck iid murl Nothing deck)
