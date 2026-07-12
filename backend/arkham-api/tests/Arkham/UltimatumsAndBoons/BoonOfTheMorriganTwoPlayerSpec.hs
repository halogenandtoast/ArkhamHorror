module Arkham.UltimatumsAndBoons.BoonOfTheMorriganTwoPlayerSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.PlayerCard (randomWeakness)
import Helpers.UltimatumsAndBoons
import TestImport.New

-- | Regression: the Morrígan return-choice used to lean on the single global
-- FocusCards list. In multiplayer every investigator's InitDeck resolves this
-- choice inside the same deck-selection window, so two of them clobbered each
-- other's focus and one player ended up with no basic weakness. The choice now
-- carries its cards in the question itself (CardLabel), so each player resolves
-- independently and both get a weakness.
spec :: Spec
spec = describe "Boon of the Morrígan (two players)" $ do
  it "gives every investigator a basic weakness" . gameTest $ \self -> do
    other <- addInvestigator Investigators.rolandBanks
    withUltimatumsAndBoons [BoonOfTheMorrigan]

    initDeckWithRandomWeakness self
    returnFirstOfferedWeakness
    initDeckWithRandomWeakness other
    returnFirstOfferedWeakness

    weaknessCount self `shouldReturn` 1
    weaknessCount other `shouldReturn` 1
 where
  initDeckWithRandomWeakness :: Investigator -> TestAppT ()
  initDeckWithRandomWeakness i = do
    placeholder <- genPlayerCard randomWeakness
    others <- testPlayerCards 2
    run $ InitDeck $ InitDeckAttrs (toId i) Nothing Nothing (Deck (placeholder : others))

  returnFirstOfferedWeakness :: TestAppT ()
  returnFirstOfferedWeakness =
    chooseOptionMatching "return weakness" \case
      CardLabel {} -> True
      _ -> False

  weaknessCount :: Investigator -> TestAppT Int
  weaknessCount i = do
    codes <- map toCardCode . unDeck <$> i.deck
    -- everything that isn't the leather coat filler and isn't an unresolved
    -- randomWeakness placeholder is the added basic weakness
    pure $ length $ filter (\c -> c /= toCardCode Assets.leatherCoat && c /= toCardCode randomWeakness) codes
