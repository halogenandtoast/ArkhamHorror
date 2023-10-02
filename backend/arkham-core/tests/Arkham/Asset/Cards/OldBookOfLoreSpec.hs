module Arkham.Asset.Cards.OldBookOfLoreSpec (spec)
where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (rolandBanks)
import TestImport.New

spec :: Spec
spec = describe "Old Book of Lore" $ do
  context "Action: exhaust old book of lore" $ do
    context "you" $ do
      it "can search the top 3 card of your deck, draw one, and shuffle remaining back in" . gameTest $ \self -> do
        deckCards <- testPlayerCards 3
        choice : others <- shuffleM deckCards
        withProp @"deck" (Deck deckCards) self
        oldBookOrLore <- self `putAssetIntoPlay` Assets.oldBookOfLore
        location <- testLocation
        self `moveTo` location
        [doSearch] <- oldBookOrLore.abilities
        self `useAbility` doSearch
        chooseTarget self
        chooseTarget (toCardId choice)
        self.hand `shouldReturn` [toCard choice]
        (unDeck <$> self.deck) `shouldMatchListM` others

    context "investigator at your location" $ do
      it "can search the top 3 card of your deck, draw one, and shuffle remaining back in" . gameTest $ \self -> do
        deckCards <- testPlayerCards 3
        choice : others <- shuffleM deckCards
        roland <- addInvestigator rolandBanks & prop @"deck" (Deck deckCards)
        oldBookOrLore <- self `putAssetIntoPlay` Assets.oldBookOfLore
        location <- testLocation
        self `moveTo` location
        roland `moveTo` location
        [doSearch] <- oldBookOrLore.abilities
        self `useAbility` doSearch
        chooseTarget roland
        chooseTarget (toCardId choice)
        roland.hand `shouldReturn` [toCard choice]
        (unDeck <$> roland.deck) `shouldMatchListM` others
