module Arkham.Types.Investigator.Cards.DaisyWalkerSpec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.Trait

spec :: Spec
spec = describe "Daisy Walker" $ do
  context "ability" $ do
    it "provides an extra Tome action" $ do
      let daisyWalker = lookupInvestigator "01002"
      gameTest
          daisyWalker
          [LoseActions (toId daisyWalker) (TestSource mempty) 3]
          id
        $ do
            runMessages
            getCanAffordCost
                (toId daisyWalker)
                (TestSource $ singleton Tome)
                Nothing
                (ActionCost 1)
              `shouldReturn` True

  context "elder sign" $ do
    it "allows you to draw one card for each Tome you control" $ do
      let daisyWalker = lookupInvestigator "01002"
      deckCards <- testPlayerCards 2
      tome1 <- testAssetWithDef (cardTraitsL .~ singleton Tome) id
      tome2 <- testAssetWithDef (cardTraitsL .~ singleton Tome) id
      gameTest
          daisyWalker
          [ SetTokens [ElderSign]
          , LoadDeck (toId daisyWalker) deckCards
          , playAsset daisyWalker tome1
          , playAsset daisyWalker tome2
          , beginSkillTest daisyWalker SkillIntellect 5
          ]
          ((assetsL %~ insertEntity tome1) . (assetsL %~ insertEntity tome2))
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            chooseOptionMatching
              "draw cards"
              (\case
                DrawCards{} -> True
                _ -> False
              )
            (handOf <$> updated daisyWalker)
              `shouldMatchListM` map PlayerCard deckCards
