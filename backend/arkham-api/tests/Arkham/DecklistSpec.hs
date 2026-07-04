{-# LANGUAGE OverloadedStrings #-}

module Arkham.DecklistSpec (spec) where

import TestImport

import Arkham.ClassSymbol
import Arkham.Decklist
import Arkham.Decklist.RandomBasicWeakness
import Arkham.Taboo.Types
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

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

  it "parses arkham.build card_pool meta" do
    parseArkhamBuildCardPool arkhamBuildChapterTwoDecklist
      `shouldBe` Just (ArkhamBuildCardPool ["cycle:core_ch2", "cycle:investigator_decks_ch2"])

  it "restricts arkham.build Chapter 2 random basic weakness candidates to Chapter 2 cards" do
    let candidates =
          randomBasicWeaknessCandidates
            RandomBasicWeaknessContext
              { rbwInvestigatorClass = Guardian
              , rbwPlayerCount = 1
              , rbwDecklist = Just arkhamBuildChapterTwoDecklist
              , rbwStandalone = False
              }
        candidateCodes = map toCardCode candidates

    candidates `shouldSatisfy` notNull
    candidateCodes `shouldSatisfy` all (.isChapterTwo)
    candidateCodes `shouldNotContain` ["02037", "04040", "07038"]

  it "falls back to the broad random basic weakness pool when card_pool is absent" do
    let candidateCodes =
          map toCardCode
            $ randomBasicWeaknessCandidates
              RandomBasicWeaknessContext
                { rbwInvestigatorClass = Guardian
                , rbwPlayerCount = 1
                , rbwDecklist = Just noCardPoolDecklist
                , rbwStandalone = False
                }

    candidateCodes `shouldContain` ["02037"]

  it "restricts non-Chapter-2 arkham.build pools by cycle token" do
    let candidateCodes =
          map toCardCode
            $ randomBasicWeaknessCandidates
              RandomBasicWeaknessContext
                { rbwInvestigatorClass = Guardian
                , rbwPlayerCount = 1
                , rbwDecklist = Just coreCardPoolDecklist
                , rbwStandalone = False
                }

    candidateCodes `shouldSatisfy` notNull
    candidateCodes `shouldSatisfy` all (T.isPrefixOf "01" . unCardCode)
    candidateCodes `shouldNotContain` ["02037"]

  it "ignores unknown arkham.build pool tokens when recognized tokens are present" do
    let candidateCodes =
          map toCardCode
            $ randomBasicWeaknessCandidates
              RandomBasicWeaknessContext
                { rbwInvestigatorClass = Guardian
                , rbwPlayerCount = 1
                , rbwDecklist = Just mixedKnownUnknownCardPoolDecklist
                , rbwStandalone = False
                }

    candidateCodes `shouldSatisfy` notNull
    candidateCodes `shouldSatisfy` all (T.isPrefixOf "01" . unCardCode)
    candidateCodes `shouldNotContain` ["02037"]

  it "falls back broadly when arkham.build pool tokens are all unknown" do
    let candidateCodes =
          map toCardCode
            $ randomBasicWeaknessCandidates
              RandomBasicWeaknessContext
                { rbwInvestigatorClass = Guardian
                , rbwPlayerCount = 1
                , rbwDecklist = Just unknownOnlyCardPoolDecklist
                , rbwStandalone = False
                }

    candidateCodes `shouldContain` ["02037"]

  it "does not widen the pool for an empty arkham.build pack token" do
    let candidateCodes =
          map toCardCode
            $ randomBasicWeaknessCandidates
              RandomBasicWeaknessContext
                { rbwInvestigatorClass = Guardian
                , rbwPlayerCount = 1
                , rbwDecklist = Just emptyPackCardPoolDecklist
                , rbwStandalone = False
                }

    candidateCodes `shouldBe` []
    candidateCodes `shouldNotContain` ["02037"]

  it "applies taboo-mutated restrictions before filtering standalone candidates" do
    let candidateCodes =
          map toCardCode
            $ randomBasicWeaknessCandidates
              RandomBasicWeaknessContext
                { rbwInvestigatorClass = Guardian
                , rbwPlayerCount = 1
                , rbwDecklist = Just noCardPoolTaboo23Decklist
                , rbwStandalone = True
                }

    candidateCodes `shouldNotContain` ["08113"]

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

arkhamBuildChapterTwoDecklist :: ArkhamDBDecklist
arkhamBuildChapterTwoDecklist =
  ArkhamDBDecklist
    { slots = Map.singleton "01000" 1
    , sideSlots = mempty
    , investigator_code = "12013"
    , investigator_name = "Isabelle Barnes"
    , meta = Just "{\"card_pool\":\"cycle:core_ch2,cycle:investigator_decks_ch2\"}"
    , taboo_id = Nothing
    , url = Just "https://api.arkham.build/v1/public/share/5917699"
    , decklist_id = Just "5917699"
    , decklist_name = Just "Isabelle Barnes Chapter Two"
    }

noCardPoolDecklist :: ArkhamDBDecklist
noCardPoolDecklist =
  arkhamBuildChapterTwoDecklist
    { meta = Just "{}"
    , investigator_code = "01001"
    , investigator_name = "Roland Banks"
    , taboo_id = Nothing
    }

coreCardPoolDecklist :: ArkhamDBDecklist
coreCardPoolDecklist = noCardPoolDecklist {meta = Just "{\"card_pool\":\"cycle:core\"}"}

mixedKnownUnknownCardPoolDecklist :: ArkhamDBDecklist
mixedKnownUnknownCardPoolDecklist = noCardPoolDecklist {meta = Just "{\"card_pool\":\"cycle:core,cycle:new\"}"}

unknownOnlyCardPoolDecklist :: ArkhamDBDecklist
unknownOnlyCardPoolDecklist = noCardPoolDecklist {meta = Just "{\"card_pool\":\"cycle:future_unknown\"}"}

emptyPackCardPoolDecklist :: ArkhamDBDecklist
emptyPackCardPoolDecklist = noCardPoolDecklist {meta = Just "{\"card_pool\":\"pack:\"}"}

noCardPoolTaboo23Decklist :: ArkhamDBDecklist
noCardPoolTaboo23Decklist = noCardPoolDecklist {taboo_id = Just 8}
