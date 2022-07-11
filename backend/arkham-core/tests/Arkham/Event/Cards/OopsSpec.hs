module Arkham.Event.Cards.OopsSpec
  ( spec
  ) where

import TestImport hiding (EnemyDamage)

import Arkham.Event.Cards qualified as Cards
import Arkham.Enemy.Attrs qualified as Enemy
import Arkham.Asset.Attrs (Field (..))
import Arkham.Enemy.Attrs (Field (..))
import Arkham.Investigator.Attrs qualified as Investigator
import Arkham.Projection

spec :: Spec
spec = describe "Oops!" $ do
  it "deals damage that attack would have done" $ do
    investigator <-
      testInvestigator
      $ (Investigator.combatL .~ 1)
      . (Investigator.resourcesL .~ 2)
    oops <- genPlayerCard Cards.oops
    rolands38Special <- buildAsset "01006" (Just investigator) -- does 2 damage
    enemy <- testEnemy $ (Enemy.healthL .~ Static 1) . (Enemy.fightL .~ 2)
    enemy2 <- testEnemy (Enemy.healthL .~ Static 3)
    location <- testLocation id

    gameTest
        investigator
        [ SetTokens [MinusOne]
        , addToHand investigator (PlayerCard oops)
        , enemySpawn location enemy
        , enemySpawn location enemy2
        , playAsset investigator rolands38Special
        , moveTo investigator location
        ]
        ((entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . enemiesL %~ insertEntity enemy2)
        . (entitiesL . locationsL %~ insertEntity location)
        . (entitiesL . assetsL %~ insertEntity rolands38Special)
        )
      $ do
          runMessages
          [doFight] <- field AssetAbilities (toId rolands38Special)
          push $ UseAbility (toId investigator) doFight []
          runMessages
          chooseOptionMatching
            "fight enemy 1"
            (\case
              FightEnemy _ eid _ _ _ _ -> eid == toId enemy
              _ -> False
            )
          chooseOptionMatching
            "start skill test"
            (\case
              StartSkillTest _ -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          chooseOptionMatching
            "play oops!"
            (\case
              TargetLabel{} -> True
              _ -> False
            )
          fieldAssert EnemyDamage (== 0) enemy
          fieldAssert EnemyDamage (== 2) enemy2

  it "[FAQ] does not deal on success damage" $ do
    investigator <-
      testInvestigator
      $ (Investigator.combatL .~ 1)
      . (Investigator.resourcesL .~ 2)
    oops <- genPlayerCard Cards.oops
    fortyOneDerringer <- buildAsset "01047" (Just investigator)
    enemy <- testEnemy $ (Enemy.healthL .~ Static 1) . (Enemy.fightL .~ 4)
    enemy2 <- testEnemy (Enemy.healthL .~ Static 3)
    location <- testLocation id

    gameTest
        investigator
        [ SetTokens [MinusOne]
        , addToHand investigator (PlayerCard oops)
        , enemySpawn location enemy
        , enemySpawn location enemy2
        , playAsset investigator fortyOneDerringer
        , moveTo investigator location
        ]
        ((entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . enemiesL %~ insertEntity enemy2)
        . (entitiesL . locationsL %~ insertEntity location)
        . (entitiesL . assetsL %~ insertEntity fortyOneDerringer)
        )
      $ do
          runMessages
          [doFight] <- field AssetAbilities (toId  fortyOneDerringer)
          push $ UseAbility (toId investigator) doFight []
          runMessages
          chooseOptionMatching
            "fight enemy 1"
            (\case
              FightEnemy _ eid _ _ _ _ -> eid == toId enemy
              _ -> False
            )
          chooseOptionMatching
            "start skill test"
            (\case
              StartSkillTest _ -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          chooseOptionMatching
            "play oops!"
            (\case
              TargetLabel{} -> True
              _ -> False
            )
          fieldAssert EnemyDamage (== 0) enemy
          fieldAssert EnemyDamage (== 1) enemy2

  it "[FAQ] shotgun only deals 1 damage" $ do
    investigator <-
      testInvestigator
      $ (Investigator.combatL .~ 1)
      . (Investigator.resourcesL .~ 2)
    oops <- genPlayerCard Cards.oops
    shotgun4 <- buildAsset "01029" (Just investigator)
    enemy <- testEnemy $ (Enemy.healthL .~ Static 1) . (Enemy.fightL .~ 5)
    enemy2 <- testEnemy (Enemy.healthL .~ Static 3)
    location <- testLocation id

    gameTest
        investigator
        [ SetTokens [MinusOne]
        , addToHand investigator (PlayerCard oops)
        , enemySpawn location enemy
        , enemySpawn location enemy2
        , playAsset investigator shotgun4
        , moveAllTo location
        ]
        ((entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . enemiesL %~ insertEntity enemy2)
        . (entitiesL . locationsL %~ insertEntity location)
        . (entitiesL . assetsL %~ insertEntity shotgun4)
        )
      $ do
          runMessages
          [doFight] <- field AssetAbilities (toId shotgun4)
          push $ UseAbility (toId investigator) doFight []
          runMessages
          chooseOptionMatching
            "fight enemy 1"
            (\case
              FightEnemy _ eid _ _ _ _ -> eid == toId enemy
              _ -> False
            )
          chooseOptionMatching
            "start skill test"
            (\case
              StartSkillTest _ -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          chooseOptionMatching
            "play oops!"
            (\case
              TargetLabel{} -> True
              _ -> False
            )
          fieldAssert EnemyDamage (== 0)  enemy
          fieldAssert EnemyDamage (== 1) enemy2
