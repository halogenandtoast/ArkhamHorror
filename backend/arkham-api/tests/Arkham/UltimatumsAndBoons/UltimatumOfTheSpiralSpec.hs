module Arkham.UltimatumsAndBoons.UltimatumOfTheSpiralSpec (spec) where

import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher qualified as Matcher
import Helpers.UltimatumsAndBoons
import TestImport.New

-- A second investigator keeps the game from ending when the first one is
-- defeated, so the queued campaign messages aren't wiped by a resolution.
spec :: Spec
spec = describe "Ultimatum of the Spiral" $ do
  context "in a campaign" $ do
    it "a defeated investigator's deck gains a random basic weakness" . gameTest $ \self -> do
      withUltimatums [UltimatumOfTheSpiral]
      asCampaign
      void $ addInvestigator Investigators.rolandBanks
      withProp @"health" 3 self
      addedWeakness <- createMessageChecker \case
        AddCampaignCardToDeck iid DoNotShuffleIn card ->
          iid == toId self && card `cardMatch` Matcher.WeaknessCard
        _ -> False
      run $ assignDamage (toId self) (TestSource mempty) 3
      applyAllDamage
      addedWeakness `refShouldBe` True

  context "standalone" $ do
    it "has no effect" . gameTest $ \self -> do
      withUltimatums [UltimatumOfTheSpiral]
      void $ addInvestigator Investigators.rolandBanks
      withProp @"health" 3 self
      addedWeakness <- createMessageChecker \case
        AddCampaignCardToDeck {} -> True
        _ -> False
      run $ assignDamage (toId self) (TestSource mempty) 3
      applyAllDamage
      addedWeakness `refShouldBe` False
