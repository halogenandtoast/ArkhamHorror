module Arkham.Investigator.Cards.DaisyWalkerSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Trait
import Arkham.Game.Helpers
import Arkham.Asset.Cards qualified as Cards
import Arkham.Timing qualified as Timing
import Arkham.Investigator.Attrs (Field (..))
import Arkham.Projection

spec :: Spec
spec = describe "Daisy Walker" $ do
  context "ability" $ do
    it "provides an extra Tome action" $ do
      let daisyWalker = lookupInvestigator "01002"
      gameTest
          daisyWalker
          [Setup, LoseActions (toId daisyWalker) (TestSource mempty) 3]
          id
        $ do
            runMessages
            getCanAffordCost
                (toId daisyWalker)
                (TestSource $ singleton Tome)
                Nothing
                [Window Timing.When $ DuringTurn $ toId daisyWalker]
                (ActionCost 1)
              `shouldReturn` True

  context "elder sign" $ do
    it "forces you to draw one card for each Tome you control" $ do
      let daisyWalker = lookupInvestigator "01002"
      deckCards <- testPlayerCards 2
      tome1 <- createAsset <$> genPlayerCard Cards.oldBookOfLore
      tome2 <- createAsset <$> genPlayerCard Cards.medicalTexts
      gameTest
          daisyWalker
          [ SetTokens [ElderSign]
          , LoadDeck (toId daisyWalker) (Deck deckCards)
          , playAsset daisyWalker tome1
          , playAsset daisyWalker tome2
          , beginSkillTest daisyWalker SkillIntellect 5
          ]
          ((entitiesL . assetsL %~ insertEntity tome1)
           . (entitiesL . assetsL %~ insertEntity tome2)
          )
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            field InvestigatorHand (toId daisyWalker) `shouldMatchListM` map PlayerCard deckCards
