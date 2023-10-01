module Arkham.Investigator.Cards.SkidsOTooleSpec (spec) where

import Arkham.Investigator.Cards
import TestImport.New

spec :: Spec
spec = describe "\"Skids\" O'Toole" $ do
  context "ability" $ do
    it "allows you to spend two resources to buy an additional action" . gameTestWith skidsOToole $ \self -> do
      self `gainResources` 2
      self `loseActions` 3
      [buyAction] <- self.abilities
      self `useAbility` buyAction
      self.resources `shouldReturn` 0
      self.remainingActions `shouldReturn` 1

  context "elder sign" $ do
    it "gains 2 resources on success" . gameTestWith skidsOToole $ \self -> do
      setChaosTokens [ElderSign]
      runSkillTest self #agility 4
      click "apply results"
      self.resources `shouldReturn` 2
