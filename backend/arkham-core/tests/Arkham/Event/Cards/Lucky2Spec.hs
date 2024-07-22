module Arkham.Event.Cards.Lucky2Spec (spec) where

import Arkham.Event.Cards qualified as Cards
import TestImport.New

spec :: Spec
spec = describe "Lucky! (2)" $ do
  it "adds 2 to a skill test when you would fail and draws 1 card" . gameTest $ \self -> do
    cardToDraw <- testPlayerCard id
    withProp @"intellect" 1 self
    withProp @"resources" 1 self
    withProp @"deck" (Deck [cardToDraw]) self
    lucky2 <- genCard Cards.lucky2
    self `addToHand` lucky2

    setChaosTokens [Zero]
    sid <- getRandom
    runSkillTest sid self #intellect 3
    assertFailedSkillTest
    chooseTarget lucky2
    self.skillValue `shouldReturn` 3
    assertPassedSkillTest
    applyResults

    self.hand `shouldReturn` [PlayerCard cardToDraw]

  it "does not cause an autofail to pass" . gameTest $ \self -> do
    cardToDraw <- testPlayerCard id
    withProp @"intellect" 1 self
    withProp @"resources" 1 self
    withProp @"deck" (Deck [cardToDraw]) self
    lucky2 <- genCard Cards.lucky2
    self `addToHand` lucky2

    setChaosTokens [AutoFail]
    sid <- getRandom
    runSkillTest sid self #intellect 2
    assertFailedSkillTest
    chooseTarget lucky2
    assertFailedSkillTest
    applyResults

    self.hand `shouldReturn` [PlayerCard cardToDraw]
