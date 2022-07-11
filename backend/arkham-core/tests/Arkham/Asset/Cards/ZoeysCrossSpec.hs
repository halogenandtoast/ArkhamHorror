module Arkham.Asset.Cards.ZoeysCrossSpec
  ( spec
  ) where

import TestImport hiding (EnemyDamage)

import Arkham.Enemy.Attrs ( Field (..) )
import Arkham.Enemy.Attrs qualified as Enemy
import Arkham.Investigator.Attrs qualified as Investigator

spec :: Spec
spec = do
  describe "Zoey's Cross" $ do
    context "after engaging an enemy" $ do
      it "spend 1 resource and exhaust to deal one damage to that enemy" $ do
        investigator <- testInvestigator (Investigator.resourcesL .~ 1)
        enemy <- testEnemy (Enemy.healthL .~ Static 2)
        location <- testLocation id
        zoeysCross <- buildAsset "02006" (Just investigator)
        gameTest
            investigator
            [ playAsset investigator zoeysCross
            , enemySpawn location enemy
            , moveTo investigator location
            ]
            ((entitiesL . assetsL %~ insertEntity zoeysCross)
            . (entitiesL . enemiesL %~ insertEntity enemy)
            . (entitiesL . locationsL %~ insertEntity location)
            )
          $ do
              runMessages
              chooseOptionMatching
                "use ability"
                (\case
                  Run{} -> True
                  _ -> False
                )
              fieldAssert EnemyDamage (== 1) enemy
