module Arkham.Types.Investigator.Cards.DaisyWalkerSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Asset.Attrs (Attrs(..))
import Arkham.Types.Trait

spec :: Spec
spec = describe "Daisy Walker" $ do
  context "ability" $ do
    it "provides an extra Tome action" $ do
      let daisyWalker = lookupInvestigator "01002"
      game <- runGameTest
        daisyWalker
        [LoseActions (toId daisyWalker) (TestSource mempty) 3]
        id
      withGame
          game
          (getCanAffordCost
            (toId daisyWalker)
            (TestSource $ singleton Tome)
            Nothing
            (ActionCost 1)
          )
        `shouldReturn` True

  context "elder sign" $ do
    it "allows you to draw one card for each Tome you control" $ do
      let daisyWalker = lookupInvestigator "01002"
      deckCards <- testPlayerCards 2
      tome1 <- testAsset $ \attrs -> attrs { assetTraits = singleton Tome }
      tome2 <- testAsset $ \attrs -> attrs { assetTraits = singleton Tome }
      game <-
        runGameTest
          daisyWalker
          [ SetTokens [ElderSign]
          , LoadDeck (toId daisyWalker) deckCards
          , playAsset daisyWalker tome1
          , playAsset daisyWalker tome2
          , beginSkillTest daisyWalker SkillIntellect 5
          ]
          ((assets %~ insertEntity tome1) . (assets %~ insertEntity tome2))
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOption "apply results"
        >>= runGameTestOptionMatching
              "draw cards"
              (\case
                DrawCards{} -> True
                _ -> False
              )
      handOf (updated game daisyWalker)
        `shouldMatchList` map PlayerCard deckCards
