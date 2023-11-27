module Arkham.Investigator.Cards.TonyMorganSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types
import Arkham.Asset.Uses
import Arkham.Investigator.Cards (tonyMorgan)
import Arkham.Projection
import TestImport.New

spec :: Spec
spec = describe "Tony Morgan" do
  it
    "You may take an additional action during your turn, which can only be used to engage or fight an enemy with 1 or more bounties on it."
    . gameTestWith tonyMorgan
    $ \self -> do
      self `putCardIntoPlay` Assets.bountyContracts
      location <- testLocation
      self `moveTo` location
      enemy <- testEnemy
      enemy `spawnAt` location
      useReaction -- place bounty
      resolveAmounts self [("Bounties", 1)]
      duringTurn self $ do
        [bountyAction] <- self `getActionsFrom` self
        self `useAbility` bountyAction
        chooseFight

  context "elder sign" do
    it "+2" . gameTestWith tonyMorgan $ \self -> do
      self.elderSignModifier `shouldReturn` PositiveModifier 2

    it "place 1 bounty on Bounty Contracts" . gameTestWith tonyMorgan $ \self -> do
      bountyContracts <- self `putAssetIntoPlay` Assets.bountyContracts
      setChaosTokens [ElderSign]
      runSkillTest self #agility 100
      -- 6 starting plus 1 from elder sign
      fieldMap AssetUses (findWithDefault 0 Bounty) bountyContracts `shouldReturn` 7
