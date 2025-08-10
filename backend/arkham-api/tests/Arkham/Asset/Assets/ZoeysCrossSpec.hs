module Arkham.Asset.Assets.ZoeysCrossSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Calculation
import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Token
import TestImport.New hiding (EnemyDamage)

spec :: Spec
spec = do
  describe "Zoey's Cross" $ do
    context "after engaging an enemy" $ do
      it "spend 1 resource and exhaust to deal one damage to that enemy" $ gameTest $ \investigator -> do
        updateInvestigator investigator (Investigator.tokensL %~ setTokens Resource 1)
        enemy <- testEnemyWith (Enemy.healthL ?~ Fixed 2)
        location <- testLocationWith id
        putCardIntoPlay investigator Assets.zoeysCross
        spawnAt enemy location
        moveTo investigator location
        chooseOptionMatching "use ability" \case
          AbilityLabel {} -> True
          _ -> False
        fieldAssert EnemyDamage (== 1) enemy
