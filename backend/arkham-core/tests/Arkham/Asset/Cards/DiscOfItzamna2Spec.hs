module Arkham.Asset.Cards.DiscOfItzamna2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.Scenario.Types
import TestImport.New

spec :: Spec
spec = describe "Disc of Itzamna (2)" $ do
  context "When a non-elite enemy spawns at your location" $ do
    it "can be discarded to discard that enemy" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      self `putCardIntoPlay` Assets.discOfItzamna2
      enemy <- testEnemy
      enemy `spawnAt` location
      useReaction
      assert $ selectNone AnyEnemy
      scenarioField ScenarioDiscard `shouldMatchListM` onlyEncounterCards [toCard enemy]

  context "When a elite enemy spawns at your location" $ do
    it "can't be used" . gameTest $ \self -> do
      location <- testLocation
      self `moveTo` location
      self `putCardIntoPlay` Assets.discOfItzamna2
      enemy <- testEnemy & elite
      enemy `spawnAt` location
      assertNoReaction
