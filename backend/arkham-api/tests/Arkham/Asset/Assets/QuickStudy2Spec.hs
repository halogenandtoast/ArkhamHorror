module Arkham.Asset.Assets.QuickStudy2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Investigator.Types qualified as Investigator
import TestImport.New

spec :: Spec
spec = describe "Quick Study (2)" $ do
  context "when its clue placement is used during a skill test" $ do
    it "can trigger Research Notes" . gameTest $ \self -> do
      quickStudy2 <- self `putAssetIntoPlay` Assets.quickStudy2
      researchNotes <- self `putAssetIntoPlay` Assets.researchNotes
      location <- testLocation
      self `moveTo` location
      updateInvestigator self (Investigator.tokensL %~ setTokens Clue 1)

      duringTurn self $ do
        sid <- getRandom
        run $ beginSkillTest sid self #intellect 0
        useFastActionOf quickStudy2 1
        useReactionOf researchNotes

      researchNotes.uses `shouldReturn` singletonMap Evidence 1

    it "can trigger Press Pass" . gameTest $ \self -> do
      quickStudy2 <- self `putAssetIntoPlay` Assets.quickStudy2
      pressPass2 <- self `putAssetIntoPlay` Assets.pressPass2
      location <- testLocation
      self `moveTo` location
      updateInvestigator self (Investigator.tokensL %~ setTokens Clue 1)

      duringTurn self $ do
        sid <- getRandom
        run $ beginSkillTest sid self #intellect 0
        useFastActionOf quickStudy2 1
        useReactionOf pressPass2
        self.remainingActions `shouldReturn` 4

      assert pressPass2.exhausted
