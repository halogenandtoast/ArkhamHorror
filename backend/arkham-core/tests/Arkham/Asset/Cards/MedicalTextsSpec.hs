module Arkham.Asset.Cards.MedicalTextsSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (rolandBanks)
import TestImport.New

spec :: Spec
spec = describe "Medical Texts" $ do
  context "choose an investigator and test 2 intellect" $ do
    let setup self roland = do
          withProp @"damage" 1 self
          location <- testLocation
          medicalTexts <- self `putAssetIntoPlay` Assets.medicalTexts
          [doHeal] <- medicalTexts.abilities
          setChaosTokens [Zero]
          self `moveTo` location
          roland `moveTo` location
          self `useAbility` doHeal

    context "if you succeed" $ do
      it "heals 1 damage from that investigator" . gameTest $ \self -> do
        withProp @"intellect" 2 self
        roland <- addInvestigator rolandBanks & prop @"damage" 1
        setup self roland

        withRewind $ do
          chooseTarget self
          startSkillTest
          applyResults
          self.damage `shouldReturn` 0
          roland.damage `shouldReturn` 1

        chooseTarget roland
        startSkillTest
        applyResults
        roland.damage `shouldReturn` 0
        self.damage `shouldReturn` 1

    context "if you fail" $ do
      it "deals 1 damage to that investigator" . gameTest $ \self -> do
        withProp @"intellect" 0 self
        roland <- addInvestigator rolandBanks & prop @"damage" 1
        setup self roland

        withRewind $ do
          chooseTarget self
          startSkillTest
          applyResults
          applyAllDamage
          self.damage `shouldReturn` 2
          roland.damage `shouldReturn` 1

        chooseTarget roland
        startSkillTest
        applyResults
        applyAllDamage
        roland.damage `shouldReturn` 2
        self.damage `shouldReturn` 1
