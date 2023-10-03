module Arkham.Asset.Cards.ScryingSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Scenario.Types (Field (..))
import TestImport.New

spec :: Spec
spec = describe "Scrying" $ do
  hasUses @"charge" Assets.scrying 3
  context "Action Ability" $ do
    context "exhausts, spends 1 charge, and" $ do
      it "look at the top 3 cards of an investigators deck and return them in any order" . gameTest $ \self -> do
        flashlight <- genPlayerCard Assets.flashlight
        knife <- genPlayerCard Assets.knife
        holyRosary <- genPlayerCard Assets.holyRosary
        withProp @"deck" (Deck [flashlight, knife, holyRosary]) self
        scrying <- self `putAssetIntoPlay` Assets.scrying
        [action] <- self `getActionsFrom` scrying
        self `useAbility` action
        chooseTarget self
        chooseTarget (toCardId knife)
        chooseTarget (toCardId flashlight)
        chooseTarget (toCardId holyRosary)
        self.deck `shouldReturn` Deck [holyRosary, flashlight, knife]
        assert scrying.exhausted
        scrying.charges `shouldReturn` 2

      it "look at the top 3 cards of the scenario deck and return them in any order" . gameTest $ \self -> do
        icyGhoul <- genEncounterCard Enemies.icyGhoul
        ghoulMinion <- genEncounterCard Enemies.ghoulMinion
        swarmOfRats <- genEncounterCard Enemies.swarmOfRats
        run $ SetEncounterDeck (Deck [icyGhoul, ghoulMinion, swarmOfRats])
        scrying <- self `putAssetIntoPlay` Assets.scrying
        [action] <- self `getActionsFrom` scrying
        self `useAbility` action
        chooseTarget EncounterDeckTarget
        chooseTarget (toCardId ghoulMinion)
        chooseTarget (toCardId icyGhoul)
        chooseTarget (toCardId swarmOfRats)
        scenarioField ScenarioEncounterDeck `shouldReturn` Deck [swarmOfRats, icyGhoul, ghoulMinion]
        assert scrying.exhausted
        scrying.charges `shouldReturn` 2
