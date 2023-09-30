module Arkham.Asset.Cards.GuardDogSpec (
  spec,
) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Types (Field (..), healthDamageL, healthL)

spec :: Spec
spec = describe "Guard Dog" $ do
  it "does 1 damage to the attacking enemy when damaged by the attack" $
    gameTest $ \investigator -> do
      enemy <- testEnemyWith ((healthDamageL .~ 1) . (healthL .~ Static 2))
      location <- testLocationWith id
      putCardIntoPlay investigator Assets.guardDog
      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ spawnAt enemy location
      pushAndRun $ moveTo investigator location
      pushAndRun EnemiesAttack
      chooseOptionMatching "damage guard dog" $ \case
        ComponentLabel (AssetComponent {}) _ -> True
        _ -> False
      chooseOptionMatching "use reaction" $ \case
        AbilityLabel {} -> True
        _ -> False
      fieldAssert EnemyDamage (== 1) enemy
