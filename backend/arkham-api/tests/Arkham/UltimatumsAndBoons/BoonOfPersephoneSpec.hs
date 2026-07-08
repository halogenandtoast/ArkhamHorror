module Arkham.UltimatumsAndBoons.BoonOfPersephoneSpec (spec) where

import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Boon of Persephone" $ do
  context "in a campaign" $ do
    it "grants a defeated investigator 3 additional experience" . gameTest $ \self -> do
      withUltimatumsAndBoons [BoonOfPersephone]
      asCampaign
      updateInvestigator self \attrs -> attrs {investigatorDefeated = True}
      tick
      getModifiers self `shouldContainM` [XPModifier "Boon of Persephone" 3]

    it "grants nothing while the investigator is not defeated" . gameTest $ \self -> do
      withUltimatumsAndBoons [BoonOfPersephone]
      asCampaign
      getModifiers self `shouldNotContainM` [XPModifier "Boon of Persephone" 3]

  context "standalone" $ do
    it "has no effect" . gameTest $ \self -> do
      withUltimatumsAndBoons [BoonOfPersephone]
      updateInvestigator self \attrs -> attrs {investigatorDefeated = True}
      tick
      getModifiers self `shouldNotContainM` [XPModifier "Boon of Persephone" 3]
