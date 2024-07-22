module Arkham.Investigator.Cards.WendyAdamsSpec (spec) where

import TestImport.New

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards

spec :: Spec
spec = describe "Wendy Adams" $ do
  context "ability" $ do
    it "allows you to discard a card to redraw a chaos token" . gameTestWith wendyAdams $ \self -> do
      card <- testPlayerCard id
      setChaosTokens [MinusOne]
      self `addToHand` PlayerCard card
      sid <- getRandom
      runSkillTest sid self #willpower 3
      useReaction
      click "discard card"
      assertPassedSkillTest

  context "elder sign" $ do
    it "gives +0" . gameTestWith wendyAdams $ \self -> do
      self.elderSignModifier `shouldReturn` PositiveModifier 0

    it "automatically succeeds if Wendy's Amulet is in play" . gameTestWith wendyAdams $ \self -> do
      -- N.B: We must run the skill test to resolve the effect of the Elder Sign
      self `putCardIntoPlay` Assets.wendysAmulet
      setChaosTokens [ElderSign]
      sid <- getRandom
      runSkillTest sid self #willpower 20
      assertPassedSkillTest
