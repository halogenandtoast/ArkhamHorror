module Arkham.UltimatumsAndBoons.BoonOfTheMorriganSpec (spec) where

import Arkham.Classes.HasGame (getGame)
import Arkham.PlayerCard (lookupPlayerCardDef, randomWeakness)
import Arkham.UltimatumsAndBoons (morriganWeaknessMessages)
import Helpers.UltimatumsAndBoons
import TestImport.New

choicesOf :: Question Message -> [UI Message]
choicesOf = \case
  QuestionLabel _ _ inner -> choicesOf inner
  ChooseOne cs -> cs
  WindowChooseOne cs -> cs
  _ -> []

{- | The three drawn weaknesses are offered as self-describing CardLabel choices
(no global FocusCards), so read the offered codes straight off the question.
-}
offeredMorriganWeaknesses :: TestAppT [CardCode]
offeredMorriganWeaknesses = do
  qs <- toList . gameQuestion <$> getGame
  pure [c | q <- qs, CardLabel c _ _ <- choicesOf q]

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

    -- Regression (#5264): 'toCardCodePairs' registers every alternate printing as its
    -- own CardDef, so Mob Enforcer sits in the basic weakness pool twice — 01101 (Core)
    -- and 01601 (Revised Core). The draw deduped on CardDef equality, which is
    -- structural, so the two printings counted as different cards and the player was
    -- offered Mob Enforcer twice.
    it "does not offer two printings of the same weakness" . gameTest $ \self -> do
      -- Mob Enforcer twice (Core then Revised Core), then two other weaknesses
      draws <- newIORef ["01101", "01601", "01099", "01098"]
      msgs <- morriganWeaknessMessages (toId self) (drawNext draws)

      [c | Ask _ q <- msgs, CardLabel c _ _ <- choicesOf q]
        `shouldMatchList` ["01101", "01099", "01098"]
 where
  drawNext :: IORef [CardCode] -> TestAppT Card
  drawNext ref =
    readIORef ref >>= \case
      [] -> error "expected at most four rigged weakness draws"
      code : rest -> writeIORef ref rest >> genCard (lookupPlayerCardDef code)
