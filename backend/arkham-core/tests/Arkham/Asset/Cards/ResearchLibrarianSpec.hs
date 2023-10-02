module Arkham.Asset.Cards.ResearchLibrarianSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "Research Librarian" $ do
  context "After it enters play" $ do
    it "lets you search your deck for a Tome asset and add it to your hand" . gameTest $ \self -> do
      otherCards <- testPlayerCards 2
      oldBookOfLore <- genPlayerCard Assets.oldBookOfLore
      deck <- Deck <$> shuffleM (oldBookOfLore : otherCards)
      withProp @"deck" deck self
      self `putCardIntoPlay` Assets.researchLibrarian
      useReaction
      chooseTarget (toCardId oldBookOfLore)
      self.hand `shouldReturn` [toCard oldBookOfLore]
      (unDeck <$> self.deck) `shouldMatchListM` otherCards
