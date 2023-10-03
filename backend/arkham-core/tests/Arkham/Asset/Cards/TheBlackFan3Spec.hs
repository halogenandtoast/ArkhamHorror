module Arkham.Asset.Cards.TheBlackFan3Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import TestImport.New

spec :: Spec
spec = describe "The Black Fan (3)" $ do
  context "with 10 or more resources" $ do
    it "gives plus 1 health and plus 1 sanity" . gameTest $ \self -> do
      withProp @"sanity" 1 self
      withProp @"health" 1 self
      self `putCardIntoPlay` Assets.theBlackFan3
      self.remainingHealth `shouldReturn` 1
      self.remainingSanity `shouldReturn` 1
      self `gainResources` 10
      self.remainingHealth `shouldReturn` 2
      self.remainingSanity `shouldReturn` 2

  context "with 15 or more resources" $ do
    it "gives one additional action" . gameTest $ \self -> do
      self `putCardIntoPlay` Assets.theBlackFan3
      self `gainResources` 15
      self.additionalActions `shouldSatisfyM` notNull
      self `spendResources` 1
      self.additionalActions `shouldSatisfyM` null

  context "with 20 or more resources" $ do
    it "gives +1 to each skill" . gameTest $ \self -> do
      withProp @"agility" 0 self
      withProp @"combat" 0 self
      withProp @"intellect" 0 self
      withProp @"willpower" 0 self
      self `putCardIntoPlay` Assets.theBlackFan3
      self.willpower `shouldReturn` 0
      self.agility `shouldReturn` 0
      self.intellect `shouldReturn` 0
      self.combat `shouldReturn` 0
      self `gainResources` 20
      self.willpower `shouldReturn` 1
      self.agility `shouldReturn` 1
      self.intellect `shouldReturn` 1
      self.combat `shouldReturn` 1
