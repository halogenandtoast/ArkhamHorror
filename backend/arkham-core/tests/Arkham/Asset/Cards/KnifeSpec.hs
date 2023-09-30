{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Arkham.Asset.Cards.KnifeSpec (
  spec,
) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Types (EnemyAttrs (..), Field (..))
import Arkham.Investigator.Types (Field (..), InvestigatorAttrs (..))
import Arkham.Matcher (assetIs)
import Arkham.Projection

spec :: Spec
spec = describe "Knife" $ do
  it "Fight. You get +1 for this attack." $ gameTest $ \investigator -> do
    updateInvestigator investigator
      $ \attrs -> attrs {investigatorCombat = 2}
    putCardIntoPlay investigator Assets.knife
    knife <- selectJust $ assetIs Assets.knife
    enemy <- testEnemyWith
      $ \attrs -> attrs {enemyHealth = Static 3, enemyFight = 3}
    location <- testLocationWith id
    pushAndRun $ SetChaosTokens [Zero]
    pushAndRun $ spawnAt enemy location
    pushAndRun $ moveTo investigator location
    [knifeFightAction, _] <- field AssetAbilities knife
    pushAndRun $ UseAbility (toId investigator) knifeFightAction []
    chooseOnlyOption "Fight enemy"
    chooseOnlyOption "Start skill test"
    chooseOnlyOption "Apply Results"
    fieldAssert EnemyDamage (== 1) enemy
    fieldAssert InvestigatorDiscard null investigator

  it
    "Discard Knife: Fight. You get +2 for this attack. This attack deals +1 damage."
    $ gameTest
    $ \investigator -> do
      updateInvestigator investigator
        $ \attrs -> attrs {investigatorCombat = 1}
      putCardIntoPlay investigator Assets.knife
      knife <- selectJust $ assetIs Assets.knife
      Just knifeCard <- preview _PlayerCard <$> field AssetCard knife
      enemy <- testEnemyWith
        $ \attrs -> attrs {enemyHealth = Static 3, enemyFight = 3}
      location <- testLocationWith id
      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ spawnAt enemy location
      pushAndRun $ moveTo investigator location
      [_, knifeDiscardFightAction] <- field AssetAbilities knife
      pushAndRun $ UseAbility (toId investigator) knifeDiscardFightAction []
      chooseOnlyOption "Fight enemy"
      chooseOnlyOption "Start skill test"
      chooseOnlyOption "Apply Results"
      fieldAssert EnemyDamage (== 2) enemy
      fieldAssert InvestigatorDiscard (== [knifeCard]) investigator
