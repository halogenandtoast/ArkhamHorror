{-# LANGUAGE OverloadedStrings #-}

module Arkham.DecklistSpec (spec) where

import TestImport

import Arkham.Decklist
import Arkham.Taboo.Types
import Data.Map.Strict qualified as Map

spec :: Spec
spec = describe "loadDecklist" $ do
  it "applies taboo to cards loaded from meta.extra_deck" $ gameTest $ \_ -> do
    decklist <- loadDecklist extraDeckDecklist

    let extraDeck = decklistExtraDeck decklist
    liftIO $ length extraDeck `shouldBe` 1
    rook <- case extraDeck of
      [x] -> pure x
      _ -> fail "expected exactly one extra deck card"
    liftIO $ rook.pcTabooList `shouldBe` Just TabooList23
    liftIO $ rook.pcMutated `shouldBe` Just "Mutated20"

  it "continues to apply taboo to cards loaded from sideSlots" $ gameTest $ \_ -> do
    decklist <- loadDecklist sideSlotsDecklist

    let extraDeck = decklistExtraDeck decklist
    liftIO $ length extraDeck `shouldBe` 1
    rook <- case extraDeck of
      [x] -> pure x
      _ -> fail "expected exactly one extra deck card"
    liftIO $ rook.pcTabooList `shouldBe` Just TabooList23
    liftIO $ rook.pcMutated `shouldBe` Just "Mutated20"

extraDeckDecklist :: ArkhamDBDecklist
extraDeckDecklist =
  ArkhamDBDecklist
    { slots = mempty
    , sideSlots = mempty
    , investigator_code = "90049"
    , investigator_name = "Jim Culver"
    , meta = Just "{\"extra_deck\":\"05153\"}"
    , taboo_id = Just 8
    , url = Nothing
    }

sideSlotsDecklist :: ArkhamDBDecklist
sideSlotsDecklist =
  ArkhamDBDecklist
    { slots = mempty
    , sideSlots = Map.singleton "05153" 1
    , investigator_code = "90049"
    , investigator_name = "Jim Culver"
    , meta = Nothing
    , taboo_id = Just 8
    , url = Nothing
    }
