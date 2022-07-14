module Arkham.Skill.Cards.ViciousBlowSpec
  ( spec
  ) where

import TestImport hiding (EnemyDamage)

import Arkham.Enemy.Attrs (Field(..), fightL, healthL)
import Arkham.Investigator.Attrs (combatL)
import Arkham.Projection
import Arkham.Skill.Cards qualified as Skills

spec :: Spec
spec = describe "Vicious Blow" $ do
  it "does 1 extra damage when attack is successful" $ do
    investigator <- testJenny (combatL .~ 1)
    enemy <- testEnemy ((healthL .~ Static 3) . (fightL .~ 1))
    location <- testLocation id
    viciousBlow <- genPlayerCard Skills.viciousBlow
    gameTest
        investigator
        [ SetTokens [Zero]
        , enemySpawn location enemy
        , moveTo investigator location
        , addToHand investigator (PlayerCard viciousBlow)
        ]
        ((entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          (fight:_) <- field EnemyAbilities (toId enemy)
          pushAndRun $ UseAbility (toId investigator) fight []
          chooseOptionMatching "commit vicious blow" $ \case
            TargetLabel (CardIdTarget _) _ -> True
            _ -> False
          chooseOptionMatching "Begin skill test" $ \case
            StartSkillTest _ -> True
            _ -> False
          chooseOnlyOption "Apply results"
          fieldAssert EnemyDamage (== 2) enemy
