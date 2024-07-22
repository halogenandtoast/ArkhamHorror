module Arkham.Investigator.Cards.PatriceHathawaySpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (patriceHathaway)
import TestImport.New

spec :: Spec
spec = describe "Patrice Hathaway" do
  it "Your maximum hand size is reduced by 3." . gameTestWith patriceHathaway $ \self -> do
    assert $ self `hasModifier` HandSize (-3)

  it
    "During each upkeep phase, instead of drawing 1 card, discard all non-weakness cards in your hand and draw until you have 5 cards in hand."
    . gameTestWith patriceHathaway
    $ \self -> do
      theTowerXVI <- genCard Assets.theTowerXVI
      flashlights <- replicateM 5 $ genCard Assets.flashlight
      knives <- replicateM 4 $ genPlayerCard Assets.knife
      withProp @"hand" (theTowerXVI : flashlights) self
      withProp @"deck" (Deck knives) self
      run $ Begin #upkeep
      for_ flashlights chooseTarget
      self.discard `shouldMatchListM` onlyPlayerCards flashlights
      self.hand `shouldMatchListM` (toCard theTowerXVI : map toCard knives)

  context "Elder Sign Effect" do
    it "gives +1" . gameTestWith patriceHathaway $ \self -> do
      self.elderSignModifier `shouldReturn` PositiveModifier 1

    it "After this test ends, you may shuffle all but 1 card from your discard pile into your deck." . gameTestWith patriceHathaway $ \self -> do
      knife <- genPlayerCard Assets.knife
      flashlights <- replicateM 4 $ genPlayerCard Assets.flashlight
      withProp @"discard" (knife : flashlights) self
      setChaosTokens [ElderSign]
      sid <- getRandom
      runSkillTest sid self #agility 100
      applyResults
      chooseFirstOption "Shuffle"
      chooseTarget knife
      self.discard `shouldReturn` [knife]
      (unDeck <$> self.deck) `shouldMatchListM` flashlights
