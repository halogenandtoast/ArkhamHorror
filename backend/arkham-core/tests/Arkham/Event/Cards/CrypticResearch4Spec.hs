module Arkham.Event.Cards.CrypticResearch4Spec (spec) where

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Cards qualified as Investigators
import TestImport.New

spec :: Spec
spec = do
  describe "Cryptic Research 4" $ do
    it "causes the selected investigator to draw 3 cards" $ gameTest $ \self -> do
      cards <- testPlayerCards 3
      location <- testLocation
      withProp @"deck" (Deck cards) self
      self `moveTo` location
      self `putCardIntoPlay` Events.crypticResearch4
      click "choose self"
      assert $ Events.crypticResearch4 `isInDiscardOf` self
      self.hand `shouldReturn` map PlayerCard cards

    it "can select any investigator at the same location" $ gameTest $ \self -> do
      investigator2 <- addInvestigator Investigators.rolandBanks
      cards <- testPlayerCards 3
      location <- testLocation
      withProp @"deck" (Deck cards) investigator2
      moveAllTo location
      self `putCardIntoPlay` Events.crypticResearch4
      chooseTarget investigator2
      assert $ Events.crypticResearch4 `isInDiscardOf` self
      investigator2.hand `shouldReturn` map PlayerCard cards
