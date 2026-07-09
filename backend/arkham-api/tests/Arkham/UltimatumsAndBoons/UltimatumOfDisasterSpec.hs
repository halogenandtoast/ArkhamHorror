module Arkham.UltimatumsAndBoons.UltimatumOfDisasterSpec (spec) where

import Arkham.Game.Settings (settingsUltimatumsAndBoonsEnabled)
import Arkham.Matcher qualified as Matcher
import Arkham.PlayerCard (randomWeakness)
import Helpers.UltimatumsAndBoons
import TestImport.New

deckWeaknesses :: Investigator -> TestAppT [PlayerCard]
deckWeaknesses self =
  filter (`cardMatch` Matcher.WeaknessCard) . unDeck <$> self.deck

spec :: Spec
spec = describe "Ultimatum of Disaster" $ do
  context "standalone" $ do
    it "adds 1 additional random basic weakness during deck initialization" . gameTest $ \self -> do
      withUltimatums [UltimatumOfDisaster]
      placeholder <- genPlayerCard randomWeakness
      others <- testPlayerCards 3
      run $ InitDeck $ InitDeckAttrs (toId self) Nothing Nothing (Deck (placeholder : others))
      -- the placeholder's base weakness plus the ultimatum's extra one
      deckWeaknesses self `shouldSatisfyM` ((== 2) . length)
      deckDefs <- asDefs self.deck
      deckDefs `shouldNotContain` [randomWeakness]

    it "a deck without the weakness placeholder still gains 1 weakness" . gameTest $ \self -> do
      withUltimatums [UltimatumOfDisaster]
      others <- testPlayerCards 3
      run $ InitDeck $ InitDeckAttrs (toId self) Nothing Nothing (Deck others)
      deckWeaknesses self `shouldSatisfyM` ((== 1) . length)

    it "has no effect while ultimatums and boons are disabled" . gameTest $ \self -> do
      withUltimatumsDisabled [UltimatumOfDisaster]
      placeholder <- genPlayerCard randomWeakness
      others <- testPlayerCards 3
      run $ InitDeck $ InitDeckAttrs (toId self) Nothing Nothing (Deck (placeholder : others))
      -- only the placeholder's base weakness
      deckWeaknesses self `shouldSatisfyM` ((== 1) . length)

    it "disabling the variant later does not retroactively change the deck" . gameTest $ \self -> do
      withUltimatums [UltimatumOfDisaster]
      others <- testPlayerCards 3
      run $ InitDeck $ InitDeckAttrs (toId self) Nothing Nothing (Deck others)
      deckWeaknesses self `shouldSatisfyM` ((== 1) . length)
      -- flip the runtime kill switch off; the already-built deck keeps the card
      setUltimatumsAndBoons \s -> s {settingsUltimatumsAndBoonsEnabled = False}
      deckWeaknesses self `shouldSatisfyM` ((== 1) . length)
