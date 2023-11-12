module Arkham.Treachery.Cards.BloodlustSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Matcher (TreacheryMatcher (TreacheryIsAttachedTo), assetIs, treacheryIs)
import Arkham.Projection
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Types (Field (..))
import TestImport.New

spec :: Spec
spec = describe "Bloodlust" do
  context "Revelation" do
    it "Remove 2 offerings from the Hungering Blade to attach Bloodlust to it" . gameTest $ \self -> do
      bloodlusts <- replicateM 3 $ genCard Treacheries.bloodlust
      theHungeringBladeCard <- genCard Assets.theHungeringBlade1
      withProp @"bonded" bloodlusts self
      withProp @"resources" 3 self
      withProp @"hand" [theHungeringBladeCard] self
      duringTurn self do
        self `playCard` theHungeringBladeCard
        theHungeringBlade <- selectJust $ assetIs Assets.theHungeringBlade1
        run $ PlaceTokens (TestSource mempty) (toTarget theHungeringBlade) Offering 2
        drawing <- Helpers.drawCards (toId self) (TestSource mempty) 1
        run drawing
        theHungeringBlade.countTokens Offering `shouldReturn` 0
        assertAny $ treacheryIs Treacheries.bloodlust <> TreacheryIsAttachedTo (toTarget theHungeringBlade)

    it "If you cannot, take 1 horror and shuffle Bloodlust back into your deck" . gameTest $ \self -> do
      bloodlusts <- replicateM 3 $ genCard Treacheries.bloodlust
      theHungeringBladeCard <- genCard Assets.theHungeringBlade1
      withProp @"bonded" bloodlusts self
      withProp @"resources" 3 self
      withProp @"hand" [theHungeringBladeCard] self
      duringTurn self do
        self `playCard` theHungeringBladeCard
        drawing <- Helpers.drawCards (toId self) (TestSource mempty) 1
        run drawing
        applyAllHorror
        self.horror `shouldReturn` 1
        (unDeck <$> self.deck) `shouldMatchListM` onlyPlayerCards bloodlusts

  context "Fast ability, while attacking with The Hungering Blade" do
    it "shuffle Bloodlust into your deck. You deal +1 damage for this attack. (Max once per attack.)"
      . gameTest
      $ \self -> do
        bloodlusts <- replicateM 3 $ genCard Treacheries.bloodlust
        theHungeringBladeCard <- genCard Assets.theHungeringBlade1
        withProp @"bonded" bloodlusts self
        withProp @"resources" 3 self
        withProp @"combat" 0 self
        withProp @"hand" [theHungeringBladeCard] self
        enemy <- testEnemy & prop @"health" 4 & prop @"fight" 0
        location <- testLocation
        self `moveTo` location
        duringTurn self do
          self `playCard` theHungeringBladeCard
          theHungeringBlade <- selectJust $ assetIs Assets.theHungeringBlade1
          run $ PlaceTokens (TestSource mempty) (toTarget theHungeringBlade) Offering 6
          drawing <- Helpers.drawCards (toId self) (TestSource mempty) 3
          run drawing
          bloodlust <- selectJust $ treacheryIs Treacheries.bloodlust
          bloodlustCard <- field TreacheryCard bloodlust
          enemy `spawnAt` location
          [doAttack] <- self `getActionsFrom` theHungeringBlade
          self `useAbility` doAttack
          chooseTarget enemy
          useFastActionOf bloodlust 1
          startSkillTest
          applyResults
          enemy.damage `shouldReturn` 3
          (unDeck <$> self.deck) `shouldReturn` onlyPlayerCards [bloodlustCard]
