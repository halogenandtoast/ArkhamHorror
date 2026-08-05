module Arkham.Campaign.ReloadDecksSpec (spec) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Deck (partitionReloadedDeck)
import Arkham.PlayerCard (lookupPlayerCardDef)
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport

-- 'ReloadDecks' reconciles the saved campaign deck against the campaign's story
-- cards at the start of every scenario: a card that is already a story card is
-- dropped from the deck so the story card's copy (which carries the persisted
-- CardId) is the one that survives.
spec :: Spec
spec = describe "ReloadDecks deck reconciliation" do
  it "drops a deck copy that is a different printing of a story card (#5346)" $ gameTest \_ -> do
    -- The engine rolled the revised core Stubborn Detective (01603) as the random
    -- basic weakness; the player hand-added the core printing (01103) to their
    -- ArkhamDB decklist. Both used to survive, growing the deck by one.
    coreDetective <- genPlayerCard Enemies.stubbornDetective
    revisedDetective <- genCard (lookupPlayerCardDef "01603")
    filler <- testPlayerCards 3

    let deck = filler <> [coreDetective]
    let (keep, dropped) = partitionReloadedDeck [revisedDetective] [] deck

    liftIO $ map toCardCode dropped `shouldBe` ["01103"]
    liftIO $ length keep `shouldBe` 3

  it "still drops a deck copy that is the same printing as the story card" $ gameTest \_ -> do
    psychosis <- genPlayerCard Treacheries.psychosis
    storyPsychosis <- genCard Treacheries.psychosis
    filler <- testPlayerCards 3

    let (keep, dropped) = partitionReloadedDeck [storyPsychosis] [] (filler <> [psychosis])

    liftIO $ map toCardCode dropped `shouldBe` ["01099"]
    liftIO $ length keep `shouldBe` 3

  it "keeps deck cards unrelated to any story card" $ gameTest \_ -> do
    deck <- testPlayerCards 3
    storyDetective <- genCard Enemies.stubbornDetective

    let (keep, dropped) = partitionReloadedDeck [storyDetective] [] deck

    liftIO $ dropped `shouldBe` []
    liftIO $ length keep `shouldBe` 3

  it "still drops invalid cards" $ gameTest \_ -> do
    -- The Dunwich Legacy sacrifices cards to Yog-Sothoth; those codes come back as
    -- 'invalidCards' and must be removed even with no story card involved.
    detective <- genPlayerCard Enemies.stubbornDetective
    filler <- testPlayerCards 3

    let (keep, dropped) = partitionReloadedDeck [] ["01103"] (filler <> [detective])

    liftIO $ map toCardCode dropped `shouldBe` ["01103"]
    liftIO $ length keep `shouldBe` 3
