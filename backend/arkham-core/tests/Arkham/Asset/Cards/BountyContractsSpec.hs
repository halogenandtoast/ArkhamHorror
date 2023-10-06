module Arkham.Asset.Cards.BountyContractsSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Types (Field (EnemyTokens))
import Arkham.Investigator.Cards (tonyMorgan)
import Arkham.Projection
import Arkham.Token
import TestImport.New

spec :: Spec
spec = describe "Bounty Contracts" do
  hasUses @"bounties" Assets.bountyContracts 6

  context "After an enemy enters play" do
    it "moves 1-3 bounties to that enemy, to a maximum of that enemy's health" . gameTestWith tonyMorgan $ \self -> do
      self `putCardIntoPlay` Assets.bountyContracts
      location <- testLocation
      self `moveTo` location
      enemy <- testEnemy & prop @"health" 2
      enemy `spawnAt` location
      useReaction -- place bounty
      assertMaxAmountChoice 2
      resolveAmounts self [("Bounties", 2)]
      fieldMap EnemyTokens (countTokens Bounty) (toId enemy) `shouldReturn` 2

  context "After you defeat an enemy with 1 or more bounties on it" do
    it "moves its bounties to your resource pool as resources" . gameTestWith tonyMorgan $ \self -> do
      self `putCardIntoPlay` Assets.bountyContracts
      location <- testLocation
      self `moveTo` location
      enemy <- testEnemy & prop @"fight" 1 & prop @"health" 2
      enemy `spawnAt` location
      useReaction -- place bounty
      resolveAmounts self [("Bounties", 2)]
      self `fightEnemy` enemy
      run $ skillTestModifier (TestSource mempty) self (DamageDealt 1)
      startSkillTest
      applyResults
      useForcedAbility
      self.resources `shouldReturn` 2
