module Arkham.Asset.Cards.Rolands38SpecialSpec (
  spec,
) where

import TestImport hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Types (EnemyAttrs (..), Field (..))
import Arkham.Investigator.Types (InvestigatorAttrs (..))
import Arkham.Location.Types (LocationAttrs (..))
import Arkham.Matcher (assetIs)
import Arkham.Projection

spec :: Spec
spec = describe "Roland's .39 Special" $ do
  it "gives +1 combat and +1 damage" $ gameTest $ \investigator -> do
    updateInvestigator investigator $
      \attrs -> attrs {investigatorCombat = 1}
    putCardIntoPlay investigator Assets.rolands38Special
    rolands38Special <- selectJust $ assetIs Assets.rolands38Special
    enemy <- testEnemy $
      \attrs -> attrs {enemyFight = 2, enemyHealth = Static 3}
    location <- testLocation id
    pushAndRun $ SetChaosTokens [Zero]
    pushAndRun $ placedLocation location
    pushAndRun $ enemySpawn location enemy
    pushAndRun $ moveTo investigator location
    [doFight] <- field AssetAbilities rolands38Special
    pushAndRun $ UseAbility (toId investigator) doFight []
    chooseOnlyOption "choose enemy"
    chooseOnlyOption "start skill test"
    chooseOnlyOption "apply results"

    fieldAssert EnemyDamage (== 2) enemy

  it
    "gives +3 combat and +1 damage if there are 1 or more clues on your location"
    $ gameTest
    $ \investigator -> do
      updateInvestigator investigator $
        \attrs -> attrs {investigatorCombat = 1}
      putCardIntoPlay investigator Assets.rolands38Special
      rolands38Special <- selectJust $ assetIs Assets.rolands38Special
      enemy <- testEnemy $
        \attrs -> attrs {enemyFight = 4, enemyHealth = Static 3}
      location <- testLocation $ \attrs -> attrs {locationClues = 1}
      pushAndRun $ SetChaosTokens [Zero]
      pushAndRun $ placedLocation location
      pushAndRun $ enemySpawn location enemy
      pushAndRun $ moveTo investigator location
      [doFight] <- field AssetAbilities rolands38Special
      pushAndRun $ UseAbility (toId investigator) doFight []
      chooseOnlyOption "choose enemy"
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      fieldAssert EnemyDamage (== 2) enemy
