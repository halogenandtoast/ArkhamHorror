module Arkham.Treachery.Cards.HauntedSpec (spec) where

import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Haunted" $ do
  it "gives -1 to each skill" . gameTest $ \self -> do
    withProp @"willpower" 1 self
    withProp @"intellect" 1 self
    withProp @"combat" 1 self
    withProp @"agility" 1 self
    location <- testLocation
    self `moveTo` location
    self `drawsCard` Treacheries.haunted
    self.willpower `shouldReturn` 0
    self.intellect `shouldReturn` 0
    self.combat `shouldReturn` 0
    self.agility `shouldReturn` 0

    haunted <- selectJust $ treacheryIs Treacheries.haunted

    [discardAction] <- self `getActionsFrom` haunted
    self `useAbility` discardAction

    self.willpower `shouldReturn` 1
    self.intellect `shouldReturn` 1
    self.combat `shouldReturn` 1
    self.agility `shouldReturn` 1
    self.remainingActions `shouldReturn` 1
