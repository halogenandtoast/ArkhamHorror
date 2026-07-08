module Arkham.UltimatumsAndBoons.BoonOfTheMorriganSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Arkham.PlayerCard (randomWeakness)
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Boon of the Morrígan" $ do
  context "standalone" $ do
    it "replaces the random basic weakness: draw 3, return 1, add 1 of the other 2 at random" . gameTest $ \self -> do
      withUltimatumsAndBoons [BoonOfTheMorrigan]
      placeholder <- genPlayerCard randomWeakness
      others <- testPlayerCards 2
      run $ InitDeck $ InitDeckAttrs (toId self) Nothing Nothing (Deck (placeholder : others))

      -- three distinct basic weaknesses are offered; return the first one
      focused <- concat . gameFocusedCards <$> getGame
      returned <- case focused of
        (c : _) -> pure c
        [] -> error "expected three focused weaknesses"
      length focused `shouldBe` 3
      let remaining = map toCardDef (drop 1 focused)
      chooseTarget returned

      -- exactly one of the two weaknesses NOT returned ends up in the deck
      deckDefs <- map toCardDef . unDeck <$> self.deck
      let added = filter (/= Assets.leatherCoat) deckDefs
      deckDefs `shouldNotContain` [randomWeakness]
      added `shouldNotContain` [toCardDef returned]
      length added `shouldBe` 1
      added `shouldSatisfy` all (`elem` remaining)
