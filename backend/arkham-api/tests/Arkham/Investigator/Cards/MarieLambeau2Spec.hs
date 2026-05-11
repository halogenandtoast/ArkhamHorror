module Arkham.Investigator.Cards.MarieLambeau2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (marieLambeau2)
import TestImport.New

addDamage :: Investigator -> Int -> TestAppT ()
addDamage i n = do
  run $ InvestigatorDirectDamage (toId i) (TestSource mempty) n 0
  applyAllDamage

spec :: Spec
spec = describe "Marie Lambeau (2)" do
  context "ability" do
    it "after damage placed on you, draw 2 cards then discard 1" . gameTestWith marieLambeau2 $ \self -> do
      [keep, drop_] <- replicateM 2 $ genPlayerCard Assets.flashlight
      withProp @"deck" (Deck [keep, drop_]) self
      self `addDamage` 1
      useReaction
      chooseTarget drop_
      self.hand `shouldMatchListM` [toCard keep]
      self.discard `shouldMatchListM` [drop_]

    it "discards even when the deck reshuffles mid-draw" . gameTestWith marieLambeau2 $ \self -> do
      -- Regression: with only 1 card in deck and 1 in discard, drawing 2
      -- triggered a reshuffle mid-draw; the AfterDrawDiscard rule was being
      -- dropped, so the player was never asked to discard.
      keep <- genPlayerCard Assets.flashlight
      drop_ <- genPlayerCard Assets.flashlight
      withProp @"deck" (Deck [keep]) self
      withProp @"discard" [drop_] self
      self `addDamage` 1
      useReaction
      chooseTarget drop_
      self.hand `shouldMatchListM` [toCard keep]
      self.discard `shouldMatchListM` [drop_]
