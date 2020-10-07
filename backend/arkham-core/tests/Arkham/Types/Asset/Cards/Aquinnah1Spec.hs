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
    enemy2 <- testEnemy (Enemy.health .~ Static 3)
    location <- testLocation "00000" id
    game <-
      runGameTest
        investigator
        [ playAsset investigator aquinnah
        , enemySpawn location enemy1
        , enemySpawn location enemy2
        , moveTo investigator location
        , enemyAttack investigator enemy1
        ]
        ((enemies %~ insertEntity enemy1)
        . (enemies %~ insertEntity enemy2)
        . (locations %~ insertEntity location)
        . (assets %~ insertEntity aquinnah)
        )
      >>= runGameTestOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOption "damage enemy2"
      >>= runGameTestOptionMatching
            "assign sanity damage to investigator"
            (\case
              Run (InvestigatorDamage{} : _) -> True
              _ -> False
            )
    updated game investigator `shouldSatisfy` hasDamage (0, 1)
    updated game enemy2 `shouldSatisfy` hasDamage (2, 0)
