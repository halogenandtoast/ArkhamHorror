module Arkham.Investigator.Cards.DaisyWalkerSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Asset.Cards qualified as Cards
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Trait

spec :: Spec
spec = describe "Daisy Walker" $ do
  context "ability" $ do
    it "provides an extra Tome action" $ gameTestWith Investigators.daisyWalker $ \daisyWalker -> do
      pushAndRunAll [Setup, LoseActions (toId daisyWalker) (TestSource mempty) 3]
      assert $
        getCanAffordCost
          (toId daisyWalker)
          (TestSource $ singleton Tome)
          Nothing
          [Window Timing.When $ DuringTurn $ toId daisyWalker]
          (ActionCost 1)

  context "elder sign" $ do
    it "forces you to draw one card for each Tome you control" $ gameTestWith Investigators.daisyWalker $ \daisyWalker -> do
      deckCards <- testPlayerCards 2
      pushAndRunAll
        [ SetTokens [ElderSign]
        , LoadDeck (toId daisyWalker) (Deck deckCards)
        ]
      putCardIntoPlay daisyWalker Cards.oldBookOfLore
      putCardIntoPlay daisyWalker Cards.medicalTexts
      pushAndRun $ beginSkillTest daisyWalker SkillIntellect 5
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      field InvestigatorHand (toId daisyWalker) `shouldMatchListM` map PlayerCard deckCards
