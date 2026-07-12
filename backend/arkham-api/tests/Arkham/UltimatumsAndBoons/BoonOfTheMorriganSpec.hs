module Arkham.UltimatumsAndBoons.BoonOfTheMorriganSpec (spec) where

import Arkham.Classes.HasGame (getGame)
import Arkham.PlayerCard (randomWeakness)
import Helpers.UltimatumsAndBoons
import TestImport.New

-- | The three drawn weaknesses are offered as self-describing CardLabel choices
-- (no global FocusCards), so read the offered codes straight off the question.
offeredMorriganWeaknesses :: TestAppT [CardCode]
offeredMorriganWeaknesses = do
  qs <- toList . gameQuestion <$> getGame
  pure [c | q <- qs, CardLabel c _ _ <- choicesOf q]
 where
  choicesOf = \case
    QuestionLabel _ _ inner -> choicesOf inner
    ChooseOne cs -> cs
    _ -> []

spec :: Spec
spec = describe "Boon of the Morrígan" $ do
  context "standalone" $ do
    it "replaces the random basic weakness: draw 3, return 1, add 1 of the other 2 at random" . gameTest $ \self -> do
      withUltimatumsAndBoons [BoonOfTheMorrigan]
      placeholder <- genPlayerCard randomWeakness
      others <- testPlayerCards 2
      run $ InitDeck $ InitDeckAttrs (toId self) Nothing Nothing (Deck (placeholder : others))

      -- three distinct basic weaknesses are offered; return the first one
      offered <- offeredMorriganWeaknesses
      length offered `shouldBe` 3
      returned <- case offered of
        (c : _) -> pure c
        [] -> error "expected three offered weaknesses"
      let remaining = filter (/= returned) offered

      chooseOptionMatching "return weakness" \case
        CardLabel c _ _ -> c == returned
        _ -> False

      -- exactly one of the two weaknesses NOT returned ends up in the deck
      deckCodes <- map toCardCode . unDeck <$> self.deck
      deckCodes `shouldNotContain` [toCardCode randomWeakness]
      deckCodes `shouldNotContain` [returned]
      let added = filter (`elem` remaining) deckCodes
      length added `shouldBe` 1
