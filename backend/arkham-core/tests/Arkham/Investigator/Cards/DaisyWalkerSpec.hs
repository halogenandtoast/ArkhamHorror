module Arkham.Investigator.Cards.DaisyWalkerSpec (spec) where

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Game.Helpers
import Arkham.Investigator.Cards (daisyWalker)
import Arkham.Trait
import TestImport.New

spec :: Spec
spec = describe "Daisy Walker" $ do
  context "constant ability" $ do
    let tomeSource = toAbilitySource (TestSource $ singleton Tome) 1
    let otherSource = toAbilitySource (TestSource mempty) 1
    let canAffordTomeAbilityCost self =
          getCanAffordCost (toId self) tomeSource Nothing [duringTurn $ toId self] (ActionCost 1)

    it "provides an extra Tome action" . gameTestWith daisyWalker $ \self -> do
      run BeginRound
      run $ LoseActions (toId self) (TestSource mempty) 3
      assert $ canAffordTomeAbilityCost self

    faq "If you have no more of those actions to lose, then you start losing “additional” actions" $ do
      it "is lost after other additional actions" . gameTestWith daisyWalker $ \self -> do
        run BeginRound
        run $ LoseActions (toId self) (TestSource mempty) 4
        assert $ not <$> canAffordTomeAbilityCost self

      it "is in the choices for additional actions to lose" . gameTestWith daisyWalker $ \self -> do
        run BeginRound
        self `putCardIntoPlay` Cards.expeditionJournal
        run $ LoseActions (toId self) (TestSource mempty) 4
        chooseOptionMatching "lose tome action" \case
          Label _ [LoseAdditionalAction _ _ (TraitRestrictedAdditionalAction Tome AbilitiesOnly)] -> True
          _ -> False
        assert $ not <$> canAffordTomeAbilityCost self
        assert
          $ getCanAffordCost (toId self) otherSource (Just #explore) [duringTurn $ toId self] (ActionCost 1)

  context "elder sign" $ do
    it "forces you to draw one card for each Tome you control" $ gameTestWith daisyWalker $ \self -> do
      deckCards <- testPlayerCards 2
      setChaosTokens [ElderSign]
      withProp @"deck" deckCards self
      self `putCardIntoPlay` Cards.oldBookOfLore
      self `putCardIntoPlay` Cards.medicalTexts
      runSkillTest self #intellect 5
      click "apply results"
      self.hand `shouldMatchListM` map toCard deckCards
