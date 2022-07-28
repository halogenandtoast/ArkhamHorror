module Arkham.Asset.Cards.GuardDogSpec
  ( spec
  ) where

import TestImport.Lifted hiding (EnemyDamage)

import qualified Arkham.Asset.Cards as Assets
import Arkham.Enemy.Types (Field(..), healthDamageL, healthL)

spec :: Spec
spec = describe "Guard Dog" $ do
  it "does 1 damage to the attacking enemy when damaged by the attack" $ do
    investigator <- testJenny id
    enemy <- testEnemy ((healthDamageL .~ 1) . (healthL .~ Static 2))
    location <- testLocation id
    guardDog <- buildAsset Assets.guardDog (Just investigator)
    gameTest
      investigator
      [ SetTokens [Zero]
      , playAsset investigator guardDog
      , enemySpawn location enemy
      , moveTo investigator location
      , EnemiesAttack
      ]
      ((entitiesL . enemiesL %~ insertEntity enemy)
      . (entitiesL . locationsL %~ insertEntity location)
      . (entitiesL . assetsL %~ insertEntity guardDog)
      )
      $ do
        runMessages
        chooseOptionMatching "damage guard dog" $ \case
          Run (AssetDamage {}:_) -> True
          _ -> False
        chooseOptionMatching "use reaction" $ \case
          Run (UseAbility {}:_) -> True
          _ -> False
        fieldAssert EnemyDamage (== 1) enemy
