module Arkham.Investigator.Cards.DaisyWalkerSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Trait

spec :: Spec
spec = describe "Daisy Walker" $ do
  context "constant ability" $ do
    it "provides an extra Tome action" $ gameTestWith Investigators.daisyWalker $ \daisyWalker -> do
      pushAndRun BeginRound
      pushAndRun $ LoseActions (toId daisyWalker) (TestSource mempty) 3
      assert $
        getCanAffordCost
          (toId daisyWalker)
          (toAbilitySource (TestSource $ singleton Tome) 1)
          Nothing
          [Window Timing.When $ DuringTurn $ toId daisyWalker]
          (ActionCost 1)

    it "is lost after other additional actions" $ gameTestWith Investigators.daisyWalker $ \daisyWalker -> do
      pushAndRun BeginRound
      pushAndRun $ LoseActions (toId daisyWalker) (TestSource mempty) 4
      assert $
        not
          <$> getCanAffordCost
            (toId daisyWalker)
            (toAbilitySource (TestSource $ singleton Tome) 1)
            Nothing
            [Window Timing.When $ DuringTurn $ toId daisyWalker]
            (ActionCost 1)

    it "is in the choices for additional actions to lose" $ gameTestWith Investigators.daisyWalker $ \daisyWalker -> do
      pushAndRun BeginRound
      putCardIntoPlay daisyWalker Cards.expeditionJournal
      pushAndRun $ LoseActions (toId daisyWalker) (TestSource mempty) 4
      chooseOptionMatching "lose tome action" \case
        Label _ [LoseAdditionalAction _ _ (TraitRestrictedAdditionalAction Tome AbilitiesOnly)] -> True
        _ -> False
      assert $
        not
          <$> getCanAffordCost
            (toId daisyWalker)
            (toAbilitySource (TestSource $ singleton Tome) 1)
            Nothing
            [Window Timing.When $ DuringTurn $ toId daisyWalker]
            (ActionCost 1)
      assert $
        getCanAffordCost
          (toId daisyWalker)
          (toAbilitySource (TestSource mempty) 2)
          (Just Action.Explore)
          [Window Timing.When $ DuringTurn $ toId daisyWalker]
          (ActionCost 1)

  context "elder sign" $ do
    it "forces you to draw one card for each Tome you control" $ gameTestWith Investigators.daisyWalker $ \daisyWalker -> do
      deckCards <- testPlayerCards 2
      pushAndRunAll
        [ SetChaosTokens [ElderSign]
        , LoadDeck (toId daisyWalker) (Deck deckCards)
        ]
      putCardIntoPlay daisyWalker Cards.oldBookOfLore
      putCardIntoPlay daisyWalker Cards.medicalTexts
      pushAndRun $ beginSkillTest daisyWalker SkillIntellect 5
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      field InvestigatorHand (toId daisyWalker) `shouldMatchListM` map PlayerCard deckCards
