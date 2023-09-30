module Arkham.Asset.Cards.Aquinnah1Spec (
  spec,
) where

import TestImport hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Investigator.Types (Field (..))

spec :: Spec
spec = describe "Aquinnah (1)" $ do
  it "can redirect damage to another enemy at your location"
    $ gameTest
    $ \investigator -> do
      enemy1 <- testEnemyWith \attrs -> attrs {Enemy.enemyHealthDamage = 2, Enemy.enemySanityDamage = 1}
      enemy2 <- testEnemyWith (Enemy.healthL .~ Static 3)
      putCardIntoPlay investigator Assets.aquinnah1
      location <- testLocationWith id
      pushAndRun $ spawnAt enemy1 location
      pushAndRun $ spawnAt enemy2 location
      pushAndRun $ moveTo investigator location
      pushAndRun $ enemyAttack investigator enemy1
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOnlyOption "damage enemy2"
      chooseOptionMatching
        "assign sanity damage to investigator"
        ( \case
            ComponentLabel InvestigatorComponent {} _ -> True
            _ -> False
        )
      fieldAssert InvestigatorHorror (== 1) investigator
      fieldAssert EnemyDamage (== 2) enemy2
