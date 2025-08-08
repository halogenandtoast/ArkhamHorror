module Arkham.Event.Events.OopsSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Calculation
import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Event.Cards qualified as Cards
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Location.Types (revealCluesL)
import Arkham.Matcher (assetIs)
import Arkham.Projection
import Arkham.Token
import TestImport.New hiding (EnemyDamage)

spec :: Spec
spec = describe "Oops!" $ do
  it "deals damage that attack would have done" $ gameTest $ \investigator -> do
    updateInvestigator investigator
      $ (Investigator.combatL .~ 1)
      . (Investigator.tokensL %~ setTokens Resource 2)
    oops <- genCard Cards.oops
    enemy <- testEnemyWith $ (Enemy.healthL ?~ Fixed 1) . (Enemy.fightL ?~ Fixed 2)
    enemy2 <- testEnemyWith (Enemy.healthL ?~ Fixed 3)
    location <- testLocationWith (revealCluesL .~ Static 0)

    setChaosTokens [MinusOne]
    addToHand investigator oops
    spawnAt enemy location
    spawnAt enemy2 location
    putCardIntoPlay investigator Assets.rolands38Special
    rolands38Special <- selectJust $ assetIs Assets.rolands38Special
    moveTo investigator location

    [doFight] <- field AssetAbilities rolands38Special
    pushAndRun $ UseAbility (toId investigator) doFight []
    chooseOptionMatching "fight enemy 1" \case
      FightLabel {enemyId} -> enemyId == toId enemy
      _ -> False
    chooseOptionMatching "start skill test" \case
      StartSkillTestButton {} -> True
      _ -> False
    chooseOnlyOption "apply results"
    chooseOptionMatching "play oops!" \case
      TargetLabel {} -> True
      _ -> False
    fieldAssert EnemyDamage (== 0) enemy
    fieldAssert EnemyDamage (== 2) enemy2

  it "[FAQ] does not deal on success damage" $ gameTest $ \investigator -> do
    updateInvestigator investigator
      $ (Investigator.combatL .~ 1)
      . (Investigator.tokensL %~ setTokens Resource 2)
    oops <- genCard Cards.oops
    enemy <- testEnemyWith $ (Enemy.healthL ?~ Fixed 1) . (Enemy.fightL ?~ Fixed 4)
    enemy2 <- testEnemyWith (Enemy.healthL ?~ Fixed 3)
    location <- testLocationWith id

    setChaosTokens [MinusOne]
    addToHand investigator oops
    spawnAt enemy location
    spawnAt enemy2 location
    putCardIntoPlay investigator Assets.fortyOneDerringer
    fortyOneDerringer <- selectJust $ assetIs Assets.fortyOneDerringer
    moveTo investigator location

    [doFight] <- field AssetAbilities fortyOneDerringer
    pushAndRun $ UseAbility (toId investigator) doFight []
    chooseOptionMatching "fight enemy 1" \case
      FightLabel {enemyId} -> enemyId == toId enemy
      _ -> False
    chooseOptionMatching "start skill test" \case
      StartSkillTestButton {} -> True
      _ -> False
    chooseOnlyOption "apply results"
    chooseOptionMatching "play oops!" \case
      TargetLabel {} -> True
      _ -> False
    fieldAssert EnemyDamage (== 0) enemy
    fieldAssert EnemyDamage (== 1) enemy2

  it "[FAQ] shotgun only deals 1 damage" . gameTest $ \investigator -> do
    updateInvestigator investigator
      $ (Investigator.combatL .~ 1)
      . (Investigator.tokensL %~ setTokens Resource 2)
    oops <- genCard Cards.oops
    enemy <- testEnemyWith $ (Enemy.healthL ?~ Fixed 1) . (Enemy.fightL ?~ Fixed 5)
    enemy2 <- testEnemyWith (Enemy.healthL ?~ Fixed 3)
    location <- testLocationWith id

    setChaosTokens [MinusOne]
    addToHand investigator oops
    spawnAt enemy location
    spawnAt enemy2 location

    putCardIntoPlay investigator Assets.shotgun4
    shotgun4 <- selectJust $ assetIs Assets.shotgun4
    moveAllTo location
    [doFight] <- field AssetAbilities shotgun4
    pushAndRun $ UseAbility (toId investigator) doFight []
    chooseOptionMatching "fight enemy 1" \case
      FightLabel {enemyId} -> enemyId == toId enemy
      _ -> False
    chooseOptionMatching "start skill test" \case
      StartSkillTestButton {} -> True
      _ -> False
    chooseOnlyOption "apply results"
    chooseOptionMatching "play oops!" \case
      TargetLabel {} -> True
      _ -> False
    fieldAssert EnemyDamage (== 0) enemy
    fieldAssert EnemyDamage (== 1) enemy2
