module Arkham.Asset.Cards.ZoeysCrossSpec (
  spec,
) where

import TestImport hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Token

spec :: Spec
spec = do
  describe "Zoey's Cross" $ do
    context "after engaging an enemy" $ do
      it "spend 1 resource and exhaust to deal one damage to that enemy" $ gameTest $ \investigator -> do
        updateInvestigator investigator (Investigator.tokensL %~ setTokens Resource 1)
        enemy <- testEnemyWith (Enemy.healthL ?~ Static 2)
        location <- testLocationWith id
        putCardIntoPlay investigator Assets.zoeysCross
        pushAndRun $ spawnAt enemy location
        pushAndRun $ moveTo investigator location
        chooseOptionMatching
          "use ability"
          ( \case
              AbilityLabel {} -> True
              _ -> False
          )
        fieldAssert EnemyDamage (== 1) enemy
