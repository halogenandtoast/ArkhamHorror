{-# LANGUAGE OverloadedStrings #-}

module Arkham.Decklist.ArkhamBuildCodesSpec (spec) where

import Api.Handler.Arkham.Decks (normalizeArkhamBuildDeckCodes)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (sort)
import Data.Text (Text)
import Test.Hspec
import Prelude

{- | arkham.build names a custom card by its own bare uuid, which has to become
the code an import of that pack gave the card.

On the raw json, because 'FromJSON CardCode' strips every leading @c@ -- the one
'ToJSON' adds so a code never serializes as a bare number -- and an arkham.build
id never had one. Jane Doe's @c13c4114-...@ decoded as @13c4114-...@, which no
longer looks like an id at all, so it went untranslated and read as a card that
does not exist (#5891).
-}
spec :: Spec
spec = describe "normalizeArkhamBuildDeckCodes" do
  it "translates an id that begins with c" do
    investigatorOf (normalizeArkhamBuildDeckCodes janeDoeDeck)
      `shouldBe` Just "*c13c41140769410c864be43f5a596c0d0"

  it "translates the cards a deck names by one" do
    slotsOf (normalizeArkhamBuildDeckCodes janeDoeDeck)
      `shouldBe` Just ["*c13c41140769410c864be43f5a596c0d0", "01087"]

  it "leaves a deck of printed cards exactly as it was" do
    normalizeArkhamBuildDeckCodes rolandDeck `shouldBe` rolandDeck

  -- The alternate front outranks the deck's own investigator, and rides along as
  -- its own blob of json rather than as a field.
  it "translates the alternate front inside meta" do
    metaOf (normalizeArkhamBuildDeckCodes janeDoeDeck)
      `shouldBe` Just "{\"alternate_front\":\"*c13c41140769410c864be43f5a596c0d0\"}"

janeDoeDeck :: Value
janeDoeDeck =
  object
    [ "investigator_code" .= ("c13c4114-0769-410c-864b-e43f5a596c0d" :: Text)
    , "investigator_name" .= ("Jane Doe" :: Text)
    , "slots" .= object ["c13c4114-0769-410c-864b-e43f5a596c0d" .= (1 :: Int), "01087" .= (2 :: Int)]
    , "meta" .= ("{\"alternate_front\":\"c13c4114-0769-410c-864b-e43f5a596c0d\"}" :: Text)
    ]

rolandDeck :: Value
rolandDeck =
  object
    [ "investigator_code" .= ("01001" :: Text)
    , "investigator_name" .= ("Roland Banks" :: Text)
    , "slots" .= object ["01020" .= (1 :: Int)]
    ]

investigatorOf :: Value -> Maybe Text
investigatorOf = field "investigator_code"

metaOf :: Value -> Maybe Text
metaOf = field "meta"

field :: FromJSON a => Key -> Value -> Maybe a
field key = \case
  Object o ->
    KeyMap.lookup key o >>= \v -> case fromJSON v of
      Success a -> Just a
      Error _ -> Nothing
  _ -> Nothing

-- Sorted: a KeyMap's own order is its hash order, which says nothing.
slotsOf :: Value -> Maybe [Text]
slotsOf = \case
  Object o -> case KeyMap.lookup "slots" o of
    Just (Object slots) -> Just $ sort $ map Key.toText (KeyMap.keys slots)
    _ -> Nothing
  _ -> Nothing
