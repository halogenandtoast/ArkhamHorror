module Arkham.Investigator.Cards.DaisyWalkerSpec (
  spec,
) where

import TestImport.New

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Game.Helpers
import Arkham.Investigator.Cards (daisyWalker)
import Arkham.Trait

spec :: Spec
spec = describe "Daisy Walker" $ do
  context "constant ability" $ do
    it "provides an extra Tome action" . gameTestWith daisyWalker $ \self -> do
      run BeginRound
      run $ LoseActions (toId self) (TestSource mempty) 3
      assert
        $ getCanAffordCost
          (toId self)
          (toAbilitySource (TestSource $ singleton Tome) 1)
          Nothing
          [duringTurn $ toId self]
          (ActionCost 1)

    it "is lost after other additional actions" . gameTestWith daisyWalker $ \self -> do
      run BeginRound
      run $ LoseActions (toId self) (TestSource mempty) 4
      assert
        $ not
        <$> getCanAffordCost
          (toId self)
          (toAbilitySource (TestSource $ singleton Tome) 1)
          Nothing
          [duringTurn $ toId self]
          (ActionCost 1)

    it "is in the choices for additional actions to lose" . gameTestWith daisyWalker $ \self -> do
      run BeginRound
      self `putCardIntoPlay` Cards.expeditionJournal
      run $ LoseActions (toId self) (TestSource mempty) 4
      chooseOptionMatching "lose tome action" \case
        Label _ [LoseAdditionalAction _ _ (TraitRestrictedAdditionalAction Tome AbilitiesOnly)] -> True
        _ -> False
      assert
        $ not
        <$> getCanAffordCost
          (toId self)
          (toAbilitySource (TestSource $ singleton Tome) 1)
          Nothing
          [duringTurn $ toId self]
          (ActionCost 1)
      assert
        $ getCanAffordCost
          (toId self)
          (toAbilitySource (TestSource mempty) 2)
          (Just #explore)
          [duringTurn $ toId self]
          (ActionCost 1)

  context "elder sign" $ do
    it "forces you to draw one card for each Tome you control" $ gameTestWith daisyWalker $ \self -> do
      deckCards <- testPlayerCards 2
      setChaosTokens [ElderSign]
      run $ LoadDeck (toId self) (Deck deckCards)
      self `putCardIntoPlay` Cards.oldBookOfLore
      self `putCardIntoPlay` Cards.medicalTexts
      run $ beginSkillTest self SkillIntellect 5
      click "start skill test"
      click "apply results"
      self.hand `shouldMatchListM` map PlayerCard deckCards
