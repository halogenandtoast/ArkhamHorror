module Arkham.Types.Asset.Cards.ZoeysCrossSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as Enemy
import qualified Arkham.Types.Investigator.Attrs as Investigator

spec :: Spec
spec = do
  describe "Zoey's Cross" $ do
    context "after engaging an enemy" $ do
      it "spend 1 resource and exhaust to deal one damage to that enemy" $ do
        investigator <- testInvestigator "00000" (Investigator.resourcesL .~ 1)
        enemy <- testEnemy (Enemy.healthL .~ Static 2)
        location <- testLocation id
        zoeysCross <- buildAsset "02006"
        gameTest
            investigator
            [ playAsset investigator zoeysCross
            , enemySpawn location enemy
            , moveTo investigator location
            ]
            ((assetsL %~ insertEntity zoeysCross)
            . (enemiesL %~ insertEntity enemy)
            . (locationsL %~ insertEntity location)
            )
          $ do
              runMessages
              chooseOptionMatching
                "use ability"
                (\case
                  Run{} -> True
                  _ -> False
                )
              updated enemy `shouldSatisfyM` hasDamage (1, 0)
