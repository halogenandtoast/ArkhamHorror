module Arkham.Asset.Cards.ZoeysCrossSpec
  ( spec
  ) where

import TestImport hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Investigator.Types qualified as Investigator

spec :: Spec
spec = do
  describe "Zoey's Cross" $ do
    context "after engaging an enemy" $ do
      it "spend 1 resource and exhaust to deal one damage to that enemy" $ gameTest $ \investigator -> do
        updateInvestigator investigator (Investigator.resourcesL .~ 1)
        enemy <- testEnemy (Enemy.healthL .~ Static 2)
        location <- testLocation id
        putCardIntoPlay investigator Assets.zoeysCross
        pushAndRun $ enemySpawn location enemy
        pushAndRun $ moveTo investigator location
        chooseOptionMatching
          "use ability"
          (\case
            AbilityLabel{} -> True
            _ -> False
          )
        fieldAssert EnemyDamage (== 1) enemy
