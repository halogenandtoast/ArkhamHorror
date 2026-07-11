module Arkham.Modifier.SearchAllInvestigatorsSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards (daisyWalker, rolandBanks)
import Arkham.Matcher
import TestImport.New

-- Covers the SearchAllInvestigators modifier (Leah Atwood Codex 2 / Fate of the
-- Vale Act 2: "Search all out-of-play areas for an Item asset and draw it").
-- The bug-prone behavior is the CROSS-OWNER draw: a search over every
-- investigator that finds an Item asset in ANOTHER investigator's deck, draws it
-- into the searcher's hand, keeps the drawn card's owner as the ORIGINAL owner,
-- and removes it from the original owner's deck (no duplication).
spec :: Spec
spec = describe "SearchAllInvestigators (Leah Atwood Codex 2)" do
  it "draws an Item asset from another investigator's deck into the searcher's hand, preserving owner"
    . gameTestWith rolandBanks
    $ \self -> do
      -- Investigator B (daisy) owns a Flashlight (Item. Tool. asset) and a Dodge
      -- (event, not an Item asset) sitting in her deck, out of play.
      daisy <- addInvestigator daisyWalker
      flashlight <- genPlayerCardWith Assets.flashlight (setPlayerCardOwner daisy.id)
      dodge <- genPlayerCardWith Events.dodge (setPlayerCardOwner daisy.id)
      withProp @"deck" (Deck [flashlight, dodge]) daisy

      -- Self (A) searches with SearchAllInvestigators active: this folds every
      -- other investigator's deck into the search under the plain FromDeck zone.
      run =<< searchModifier (TestSource mempty) self.id SearchAllInvestigators
      run
        $ search
          self.id
          (TestSource mempty)
          self.id
          [fromDeck]
          (basic (#item <> #asset))
          (DrawFound self.id 1)

      -- Only the Item asset is offered; the non-Item event is filtered out.
      assertTarget (toCard flashlight)
      assertNotTarget (toCard dodge)
      chooseTarget (toCard flashlight)

      -- 1. The Item asset ends up in the searcher's hand.
      self.hand `shouldReturn` [toCard flashlight]
      -- 2. The drawn card's owner is still B (pcOwner preserved), not the searcher.
      handOwners <- map toCardOwner <$> self.hand
      handOwners `shouldBe` [Just daisy.id]
      -- 3. It is gone from B's deck (no duplication); the non-Item card stays.
      (unDeck <$> daisy.deck) `shouldMatchListM` [dodge]
