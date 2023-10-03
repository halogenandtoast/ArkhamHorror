module Arkham.Asset.Cards.PickpocketingSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import Arkham.Matcher
import Arkham.Scenario.Types
import TestImport.New

spec :: Spec
spec = describe "Pickpocketing" $ do
  context "After you evade an enemy" $ do
    it "can be exhausted to draw 1 card" . gameTest $ \self -> do
      cards <- testPlayerCards 1
      enemy <- testEnemy & prop @"evade" 0
      location <- testLocation
      withProp @"agility" 1 self
      withProp @"deck" (Deck cards) self
      setChaosTokens [Zero]
      self `putCardIntoPlay` Assets.pickpocketing
      self `moveTo` location
      enemy `spawnAt` location
      self `evadeEnemy` enemy
      startSkillTest
      applyResults
      useReaction
      self.hand `shouldReturn` map toCard cards

    faq "If you use this to draw a card with the same triggering condition (e.g. Close Call)" $ do
      it "you can play that card during the same reaction window." . gameTest $ \self -> do
        enemy <- testEnemy & prop @"evade" 0
        location <- testLocation
        closeCall2 <- genPlayerCard Events.closeCall2
        withProp @"agility" 1 self
        withProp @"resources" 2 self
        withProp @"deck" (Deck [closeCall2]) self
        setChaosTokens [Zero]
        self `putCardIntoPlay` Assets.pickpocketing
        self `moveTo` location
        enemy `spawnAt` location
        self `evadeEnemy` enemy
        startSkillTest
        applyResults
        useReaction
        chooseTarget (toCardId closeCall2)
        assert $ selectNone AnyEnemy
        scenarioField ScenarioEncounterDeck `shouldReturn` Deck (onlyEncounterCards [toCard enemy])
