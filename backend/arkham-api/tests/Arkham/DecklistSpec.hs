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

  it "preserves arkham.build hunch deck attachments from meta" $ gameTest $ \_ -> do
    let attachments = decklistAttachments arkhamBuildHunchDecklist

    liftIO
      $ attachments
      `shouldBe` Map.singleton
        "05002"
        ["05010", "03026", "03026", "04103", "04103", "02186", "02186", "01022", "01022", "01037", "01037"]

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
    , decklist_id = Nothing
    , decklist_name = Nothing
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
    , decklist_id = Nothing
    , decklist_name = Nothing
    }

arkhamBuildHunchDecklist :: ArkhamDBDecklist
arkhamBuildHunchDecklist =
  ArkhamDBDecklist
    { slots = mempty
    , sideSlots = mempty
    , investigator_code = "05002"
    , investigator_name = "Joe Diamond"
    , meta = Just "{\"attachments_05002\":\"05010,03026,03026,04103,04103,02186,02186,01022,01022,01037,01037\"}"
    , taboo_id = Nothing
    , url = Nothing
    , decklist_id = Nothing
    , decklist_name = Nothing
    }
