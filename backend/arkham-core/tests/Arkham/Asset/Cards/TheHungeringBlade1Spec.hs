module Arkham.Asset.Cards.TheHungeringBlade1Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Matcher (assetIs)
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "The Hungering Blade (1)" do
  context
    "As an additional cost to play this card, you must search your bonded cards for 3 copies of Bloodlust and shuffle them into your deck."
    do
      it "can be played if cards are in bonded" . gameTest $ \self -> do
        theHungeringBlade <- genCard Assets.theHungeringBlade1
        bloodlusts <- replicateM 3 $ genCard Treacheries.bloodlust
        withProp @"resources" 3 self
        withProp @"bonded" bloodlusts self
        withProp @"hand" [theHungeringBlade] self
        duringTurn self do
          self.playableCards `shouldReturn` [theHungeringBlade]
          self `playCard` theHungeringBlade
          (unDeck <$> self.deck) `shouldMatchListM` onlyPlayerCards bloodlusts

      it "can't be played if cards are not in bonded" . gameTest $ \self -> do
        theHungeringBlade <- genCard Assets.theHungeringBlade1
        withProp @"resources" 3 self
        withProp @"hand" [theHungeringBlade] self
        duringTurn self do
          self.playableCards `shouldReturn` []

  context "fight action" do
    it "get +1 combat for this attack for each attached copy of Bloodlust" . gameTest $ \self -> do
      bloodlusts <- replicateM 3 $ genCard Treacheries.bloodlust
      theHungeringBladeCard <- genCard Assets.theHungeringBlade1
      withProp @"bonded" bloodlusts self
      withProp @"combat" 0 self
      withProp @"hand" [theHungeringBladeCard] self
      enemy <- testEnemy & prop @"health" 3 & prop @"fight" 0
      location <- testLocation
      self `moveTo` location
      duringTurn self do
        self `playCard` theHungeringBladeCard
        theHungeringBlade <- selectJust $ assetIs Assets.theHungeringBlade1
        run $ PlaceTokens (TestSource mempty) (toTarget theHungeringBlade) Offering 6
        drawing <- Helpers.drawCards (toId self) (TestSource mempty) 3
        run drawing
        enemy `spawnAt` location
        [doAttack] <- self `getActionsFrom` theHungeringBlade
        self `useAbility` doAttack
        chooseTarget enemy
        skip -- skip all bloodlust fast abilities
        startSkillTest
        self.combat `shouldReturn` 3

    it "This attack deals +1 damage" . gameTest $ \self -> do
      bloodlusts <- replicateM 3 $ genCard Treacheries.bloodlust
      enemy <- testEnemy & prop @"health" 3 & prop @"fight" 0
      location <- testLocation
      self `moveTo` location
      enemy `spawnAt` location
      withProp @"bonded" bloodlusts self
      theHungeringBlade <- self `putAssetIntoPlay` Assets.theHungeringBlade1
      duringTurn self do
        [doAttack] <- self `getActionsFrom` theHungeringBlade
        self `useAbility` doAttack
        chooseTarget enemy
        startSkillTest
        applyResults
        enemy.damage `shouldReturn` 2

    it
      "If this attack defeats an enemy, place 1 resource on this card (from the token bank), as an offering."
      . gameTest
      $ \self -> do
        bloodlusts <- replicateM 3 $ genCard Treacheries.bloodlust
        enemy <- testEnemy & prop @"health" 1 & prop @"fight" 0
        location <- testLocation
        self `moveTo` location
        enemy `spawnAt` location
        withProp @"bonded" bloodlusts self
        theHungeringBlade <- self `putAssetIntoPlay` Assets.theHungeringBlade1
        duringTurn self do
          [doAttack] <- self `getActionsFrom` theHungeringBlade
          self `useAbility` doAttack
          chooseTarget enemy
          startSkillTest
          applyResults
          theHungeringBlade.countTokens Offering `shouldReturn` 1
