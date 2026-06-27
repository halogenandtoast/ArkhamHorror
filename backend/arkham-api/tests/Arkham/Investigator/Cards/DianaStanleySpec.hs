module Arkham.Investigator.Cards.DianaStanleySpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher (assetIs)
import Arkham.Modifier
import Arkham.Projection
import Arkham.Window qualified as Window
import TestImport.New

-- Diana Stanley's reaction: "After a card you own cancels or ignores a card effect or game
-- effect, if there are fewer than 5 cards beneath Diana Stanley: Place that card facedown
-- beneath her. Draw 1 card and gain 1 resource."
--
-- The cancelling source must be a *card you own* -- not merely an effect you performed. The
-- distinction matters because activating a location's ability (e.g. The Cabildo) is an effect
-- you perform but is not a card you own. Regression coverage for #4902 (Scarlet Keys:
-- Sanguine Shadows), where reacting to The Cabildo's cancel crashed on an unhandled source.

cancelWindow :: Sourceable source => source -> Message
cancelWindow source =
  CheckWindows [Window.mkAfter $ Window.CancelledOrIgnoredCardOrGameEffect (toSource source) Nothing]

spec :: Spec
spec = describe "Diana Stanley" do
  context "reaction (cancel/ignore window)" do
    it "triggers when a card you own cancels an effect"
      . gameTestWith Investigators.dianaStanley
      $ \self -> do
        self `loadDeck` [Assets.flashlight]
        leatherCoat <- putAssetIntoPlay self Assets.leatherCoat
        run $ cancelWindow (AssetSource leatherCoat)
        useReaction
        -- gains a resource, draws a card, and places the cancelling card beneath her
        self.resources `shouldReturn` 1
        fieldMap InvestigatorCardsUnderneath length (toId self) `shouldReturn` 1
        selectAny (assetIs Assets.leatherCoat) `shouldReturn` False

    it "does not trigger when a location ability cancels an effect (#4902)"
      . gameTestWith Investigators.dianaStanley
      $ \_self -> do
        location <- testLocation
        run $ cancelWindow (AbilitySource (LocationSource $ toId location) 1)
        assertNoReaction

    it "does not trigger when another investigator's card cancels an effect"
      . gameTestWith Investigators.dianaStanley
      $ \_self -> do
        roland <- addInvestigator Investigators.rolandBanks
        theirAsset <- putAssetIntoPlay roland Assets.leatherCoat
        run $ cancelWindow (AssetSource theirAsset)
        assertNoReaction

    it "does not trigger from your own investigator source"
      . gameTestWith Investigators.dianaStanley
      $ \self -> do
        run $ cancelWindow (InvestigatorSource $ toId self)
        assertNoReaction

  context "ignoring an enemy keyword (.45 Automatic (2))" do
    it "triggers when the attack is declared against a retaliate enemy"
      . gameTestWith Investigators.dianaStanley
      $ \self -> do
        self `loadDeck` [Assets.flashlight]
        withProp @"combat" 1 self
        fortyFive <- self `putAssetIntoPlay` Assets.fortyFiveAutomatic2
        enemy <- testEnemy & prop @"fight" 2 & prop @"health" 3
        location <- testLocation
        enemy `spawnAt` location
        self `moveTo` location
        run =<< gameModifier (TestSource mempty) (toTarget enemy) (AddKeyword Keyword.Retaliate)
        [doFight] <- self `getActionsFrom` fortyFive
        self `useAbility` doFight
        -- the window opens when the attack is declared, before the test resolves
        chooseTarget enemy
        useReaction
        -- gains a resource, draws a card, and places the .45 Automatic (2) beneath her
        self.resources `shouldReturn` 1
        fieldMap InvestigatorCardsUnderneath length (toId self) `shouldReturn` 1
        selectAny (assetIs Assets.fortyFiveAutomatic2) `shouldReturn` False

    it "does not trigger when the enemy has no retaliate to ignore"
      . gameTestWith Investigators.dianaStanley
      $ \self -> do
        self `loadDeck` [Assets.flashlight]
        withProp @"combat" 1 self
        fortyFive <- self `putAssetIntoPlay` Assets.fortyFiveAutomatic2
        enemy <- testEnemy & prop @"fight" 2 & prop @"health" 3
        location <- testLocation
        enemy `spawnAt` location
        self `moveTo` location
        [doFight] <- self `getActionsFrom` fortyFive
        self `useAbility` doFight
        chooseTarget enemy
        assertNoReaction
