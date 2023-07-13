module Arkham.Skill.Cards.ViciousBlowSpec (
  spec,
) where

import TestImport hiding (EnemyDamage)

import Arkham.Enemy.Types (Field (..), fightL, healthL)
import Arkham.Investigator.Types (combatL)
import Arkham.Projection
import Arkham.Skill.Cards qualified as Skills

spec :: Spec
spec = describe "Vicious Blow" $ do
  it "does 1 extra damage when attack is successful" $ gameTest $ \investigator -> do
    updateInvestigator investigator (combatL .~ 1)
    enemy <- testEnemy ((healthL .~ Static 3) . (fightL .~ 1))
    location <- testLocation id
    viciousBlow <- genCard Skills.viciousBlow
    pushAndRunAll
      [ SetChaosTokens [Zero]
      , enemySpawn location enemy
      , moveTo investigator location
      , addToHand (toId investigator) viciousBlow
      ]
    (fight : _) <- field EnemyAbilities (toId enemy)
    pushAndRun $ UseAbility (toId investigator) fight []
    chooseOptionMatching "commit vicious blow" $ \case
      TargetLabel (CardIdTarget _) _ -> True
      _ -> False
    chooseOptionMatching "Begin skill test" $ \case
      StartSkillTestButton {} -> True
      _ -> False
    chooseOnlyOption "Apply results"
    fieldAssert EnemyDamage (== 2) enemy
