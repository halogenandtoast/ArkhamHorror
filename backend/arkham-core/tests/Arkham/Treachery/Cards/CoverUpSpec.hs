{-# OPTIONS_GHC -Wno-type-defaults #-}

module Arkham.Treachery.Cards.CoverUpSpec (spec) where

import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import TestImport.New

default (Int)

spec :: Spec
spec = describe "Cover Up" $ do
  it "starts with 3 clues on it" $ gameTest $ \self -> do
    self `loadDeck` [Cards.coverUp]
    self `drawCards` 1
    coverUp <- selectJust $ treacheryIs Cards.coverUp
    coverUp.clues `shouldReturn` 3

  it "allows you to remove a clue instead of discovering clues" $ gameTest $ \self -> do
    location <- testLocation & prop @"clues" 1
    self `loadDeck` [Cards.coverUp]
    self `drawCards` 1
    self `moveTo` location
    self `discoverClues` 1
    useReaction
    coverUp <- selectJust $ treacheryIs Cards.coverUp
    coverUp.clues `shouldReturn` 2
    location.clues `shouldReturn` 1

  it "causes one mental trauma when the game ends if there are any clues on it" $ gameTest $ \self -> do
    self `loadDeck` [Cards.coverUp]
    self `drawCards` 1
    run $ EndOfGame Nothing
    click "trigger cover up"
    self.mentalTrauma `shouldReturn` 1

  it "does not cause trauma when the game ends if there are no clues on it" $ gameTest $ \self -> do
    location <- testLocation & prop @"clues" 3
    self `loadDeck` [Cards.coverUp]
    self `moveTo` location
    self `drawCards` 1
    self `discoverClues` 3
    useReaction
    run $ EndOfGame Nothing
    coverUp <- selectJust $ treacheryIs Cards.coverUp
    coverUp.clues `shouldReturn` 0
    self.mentalTrauma `shouldReturn` 0
