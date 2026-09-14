module Arkham.Treachery.Cards.TheDreamEaters.TheSearchForKadath.DreamlandsEclipseSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.GameEnv (getJustSkillTest)
import Arkham.Helpers.SkillTest (getModifiedSkillTestDifficulty)
import Arkham.Treachery.CardDefs.TheDreamEaters.TheSearchForKadath qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Dreamlands Eclipse" $ do
  context "forced ability during an Ancient Stone investigation" $ do
    it "applies +2 shroud to the investigated location" . gameTest $ \self -> do
      ancientStone <- self `putAssetIntoPlay` Assets.ancientStone1
      void $ self `putTreacheryIntoPlay` Treacheries.dreamlandsEclipse
      location <- testLocation & prop @"shroud" 4 & prop @"clues" 1
      self `moveTo` location

      duringTurn self do
        [investigateAction] <- self `getActionsFrom` ancientStone
        self `useAbility` investigateAction
        useForcedAbility
        clickLabel "$theDreamEaters.theSearchForKadath.dreamlandsEclipse.label.shroudPlus2"

        skillTest <- getJustSkillTest
        difficulty <- getModifiedSkillTestDifficulty skillTest
        difficulty `shouldBe` 9

    it "leaves the difficulty unchanged when taking 1 horror instead" . gameTest $ \self -> do
      ancientStone <- self `putAssetIntoPlay` Assets.ancientStone1
      void $ self `putTreacheryIntoPlay` Treacheries.dreamlandsEclipse
      location <- testLocation & prop @"shroud" 4 & prop @"clues" 1
      self `moveTo` location

      duringTurn self do
        [investigateAction] <- self `getActionsFrom` ancientStone
        self `useAbility` investigateAction
        useForcedAbility
        clickLabel "$label.takeHorror"

        skillTest <- getJustSkillTest
        difficulty <- getModifiedSkillTestDifficulty skillTest
        difficulty `shouldBe` 7
