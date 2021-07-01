module Arkham.Types.Asset.Cards.Aquinnah1Spec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as Enemy

spec :: Spec
spec = describe "Aquinnah (1)" $ do
  it "can redirect damage to another enemy at your location" $ do
    aquinnah <- buildAsset "01082"
    investigator <- testInvestigator "00000" id
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
        ((enemiesL %~ insertEntity enemy1)
        . (enemiesL %~ insertEntity enemy2)
        . (locationsL %~ insertEntity location)
        . (assetsL %~ insertEntity aquinnah)
        )
      $ do
          runMessages
          chooseOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
          chooseOnlyOption "damage enemy2"
          chooseOptionMatching
            "assign sanity damage to investigator"
            (\case
              Run (InvestigatorDamage{} : _) -> True
              _ -> False
            )
          updated investigator `shouldSatisfyM` hasDamage (0, 1)
          updated enemy2 `shouldSatisfyM` hasDamage (2, 0)
