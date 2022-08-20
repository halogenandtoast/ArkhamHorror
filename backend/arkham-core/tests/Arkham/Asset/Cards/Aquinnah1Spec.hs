module Arkham.Asset.Cards.Aquinnah1Spec
  ( spec
  ) where

import TestImport hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Investigator.Types (Field (..))
import Arkham.Enemy.Types (Field (..))

spec :: Spec
spec = describe "Aquinnah (1)" $ do
  it "can redirect damage to another enemy at your location" $ do
    investigator <- testJenny id
    aquinnah <- buildAsset Assets.aquinnah1 (Just investigator)
    enemy1 <- testEnemy
      (\attrs ->
        attrs { Enemy.enemyHealthDamage = 2, Enemy.enemySanityDamage = 1 }
      )
    enemy2 <- testEnemy (Enemy.healthL .~ Static 3)
    location <- testLocation id
    gameTest
        investigator
        [ playAsset investigator aquinnah
        , enemySpawn location enemy1
        , enemySpawn location enemy2
        , moveTo investigator location
        , enemyAttack investigator enemy1
        ]
        ((entitiesL . enemiesL %~ insertEntity enemy1)
        . (entitiesL . enemiesL %~ insertEntity enemy2)
        . (entitiesL . locationsL %~ insertEntity location)
        . (entitiesL . assetsL %~ insertEntity aquinnah)
        )
      $ do
          runMessages
          chooseOptionMatching
            "use ability"
            (\case
              AbilityLabel{} -> True
              _ -> False
            )
          chooseOnlyOption "damage enemy2"
          chooseOptionMatching
            "assign sanity damage to investigator"
            (\case
              ComponentLabel InvestigatorComponent{} _ -> True
              _ -> False
            )
          fieldAssert InvestigatorHorror (== 1) investigator
          fieldAssert EnemyDamage (== 2) enemy2
