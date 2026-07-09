module Arkham.UltimatumsAndBoons.UltimatumOfFinalitySpec (spec) where

import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher qualified as Matcher
import Helpers.UltimatumsAndBoons
import TestImport.New

-- A second investigator keeps the game from ending (and the scenario from
-- resolving) when the first one is defeated, so the assertions run against a
-- stable game state.
spec :: Spec
spec = describe "Ultimatum of Finality" $ do
  context "in a campaign" $ do
    it "an investigator defeated by damage is killed" . gameTest $ \self -> do
      withUltimatums [UltimatumOfFinality]
      asCampaign
      void $ addInvestigator Investigators.rolandBanks
      withProp @"health" 3 self
      run $ assignDamage (toId self) (TestSource mempty) 3
      applyAllDamage
      self.defeated `shouldReturn` True
      assertAny Matcher.KilledInvestigator
      assertNone Matcher.InsaneInvestigator

    it "an investigator defeated by horror is driven insane" . gameTest $ \self -> do
      withUltimatums [UltimatumOfFinality]
      asCampaign
      void $ addInvestigator Investigators.rolandBanks
      withProp @"sanity" 3 self
      run $ assignHorror (toId self) (TestSource mempty) 3
      applyAllHorror
      self.defeated `shouldReturn` True
      assertAny Matcher.InsaneInvestigator
      assertNone Matcher.KilledInvestigator

  context "standalone" $ do
    it "has no effect" . gameTest $ \self -> do
      withUltimatums [UltimatumOfFinality]
      void $ addInvestigator Investigators.rolandBanks
      withProp @"health" 3 self
      run $ assignDamage (toId self) (TestSource mempty) 3
      applyAllDamage
      self.defeated `shouldReturn` True
      assertNone Matcher.KilledInvestigator
      assertNone Matcher.InsaneInvestigator
