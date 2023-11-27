module Arkham.Asset.Cards.Tonys38LongColtSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetUses))
import Arkham.Asset.Uses
import Arkham.Investigator.Cards (tonyMorgan)
import Arkham.Matcher
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "Tony's .38 Long Colt" do
  hasUses @"ammo" Assets.tonys38LongColt 3

  context "After you play Tony's .38 Long Colt" do
    it "Play another from your hand at no cost" . gameTestWith tonyMorgan $ \self -> do
      tonys38LongColt <- genCard Assets.tonys38LongColt
      otherTonys38LongColt <- genCard Assets.tonys38LongColt
      withProp @"hand" [tonys38LongColt, otherTonys38LongColt] self
      withProp @"resources" 3 self
      self `playCard` tonys38LongColt
      self.resources `shouldReturn` 0
      useReaction
      selectCount (assetIs Assets.tonys38LongColt) `shouldReturn` 2
      self.hand `shouldReturn` []

  context "Fight action" do
    it "You get +1 for each bounty on the attacked enemy." . gameTestWith tonyMorgan $ \self -> do
      withProp @"combat" 0 self
      self `putCardIntoPlay` Assets.bountyContracts
      tonys38LongColt <- self `putAssetIntoPlay` Assets.tonys38LongColt
      location <- testLocation
      self `moveTo` location
      enemy <- testEnemy & prop @"fight" 2 & prop @"health" 2
      enemy `spawnAt` location
      useReaction -- place bounty
      assertMaxAmountChoice 2
      resolveAmounts self [("Bounties", 2)]
      [doFight] <- self `getActionsFrom` tonys38LongColt
      self `useAbility` doFight
      chooseTarget enemy
      startSkillTest
      assertPassedSkillTest

    it "This attack deals +1 damage" . gameTestWith tonyMorgan $ \self -> do
      tonys38LongColt <- self `putAssetIntoPlay` Assets.tonys38LongColt
      location <- testLocation
      self `moveTo` location
      enemy <- testEnemy & prop @"fight" 0 & prop @"health" 3
      enemy `spawnAt` location
      [doFight] <- self `getActionsFrom` tonys38LongColt
      self `useAbility` doFight
      chooseTarget enemy
      startSkillTest
      applyResults
      enemy.damage `shouldReturn` 2

    it
      "If this attack defeats an enemy with 1 or more bounties on it, place 1 bounty on Bounty Contracts"
      . gameTestWith tonyMorgan
      $ \self -> do
        withProp @"combat" 0 self
        bountyContracts <- self `putAssetIntoPlay` Assets.bountyContracts
        tonys38LongColt <- self `putAssetIntoPlay` Assets.tonys38LongColt
        location <- testLocation
        self `moveTo` location
        enemy <- testEnemy & prop @"fight" 2 & prop @"health" 2
        enemy `spawnAt` location
        useReaction -- place bounty
        assertMaxAmountChoice 2
        resolveAmounts self [("Bounties", 2)]
        [doFight] <- self `getActionsFrom` tonys38LongColt
        self `useAbility` doFight
        chooseTarget enemy
        startSkillTest
        applyResults
        -- started with 6, moved 2, gain 1: 6 - 2 + 1 = 5
        fieldMap AssetUses (findWithDefault 0 Bounty) bountyContracts `shouldReturn` 5
