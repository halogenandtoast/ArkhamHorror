module Arkham.UltimatumsAndBoons.UltimatumOfInductionSpec (spec) where

import Arkham.Helpers.Xp (getXp)
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Ultimatum of Induction" $ do
  context "in a campaign" $ do
    it "investigators cannot gain experience" . gameTest $ \self -> do
      withUltimatums [UltimatumOfInduction]
      asCampaign
      getModifiers self `shouldContainM` [CannotGainXP]
      getXp `shouldReturn` []

  context "standalone" $ do
    it "has no effect" . gameTest $ \self -> do
      withUltimatums [UltimatumOfInduction]
      getModifiers self `shouldNotContainM` [CannotGainXP]
      getXp `shouldReturn` [(toId self, 0)]
