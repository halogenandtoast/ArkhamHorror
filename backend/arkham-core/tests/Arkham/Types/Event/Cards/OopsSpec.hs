module Arkham.Types.Event.Cards.OopsSpec
  ( spec
  ) where

import TestImport

import qualified Arkham.Event.Cards as Cards
import qualified Arkham.Types.Enemy.Attrs as Enemy
import qualified Arkham.Types.Investigator.Attrs as Investigator

spec :: Spec
spec = describe "Oops!" $ do
  it "deals damage that attack would have done" $ do
    investigator <-
      testInvestigator "00000"
      $ (Investigator.combatL .~ 1)
      . (Investigator.resourcesL .~ 2)
    oops <- genPlayerCard Cards.oops
    rolands38Special <- buildAsset "01006" -- does 2 damage
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
        ((enemiesL %~ insertEntity enemy)
        . (enemiesL %~ insertEntity enemy2)
        . (locationsL %~ insertEntity location)
        . (assetsL %~ insertEntity rolands38Special)
        )
      $ do
          runMessages
          [doFight] <- getAbilitiesOf rolands38Special
          push $ UseAbility (toId investigator) doFight []
          runMessages
          chooseOptionMatching
            "fight enemy 1"
            (\case
              FightEnemy _ eid _ _ _ -> eid == toId enemy
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
              Run{} -> True
              _ -> False
            )
          updated enemy `shouldSatisfyM` hasDamage (0, 0)
          updated enemy2 `shouldSatisfyM` hasDamage (2, 0)

  it "[FAQ] does not deal on success damage" $ do
    investigator <-
      testInvestigator "00000"
      $ (Investigator.combatL .~ 1)
      . (Investigator.resourcesL .~ 2)
    oops <- genPlayerCard Cards.oops
    fortyOneDerringer <- buildAsset "01047"
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
        ((enemiesL %~ insertEntity enemy)
        . (enemiesL %~ insertEntity enemy2)
        . (locationsL %~ insertEntity location)
        . (assetsL %~ insertEntity fortyOneDerringer)
        )
      $ do
          runMessages
          [doFight] <- getAbilitiesOf fortyOneDerringer
          push $ UseAbility (toId investigator) doFight []
          runMessages
          chooseOptionMatching
            "fight enemy 1"
            (\case
              FightEnemy _ eid _ _ _ -> eid == toId enemy
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
              Run{} -> True
              _ -> False
            )
          updated enemy `shouldSatisfyM` hasDamage (0, 0)
          updated enemy2 `shouldSatisfyM` hasDamage (1, 0)

  it "[FAQ] shotgun only deals 1 damage" $ do
    investigator <-
      testInvestigator "00000"
      $ (Investigator.combatL .~ 1)
      . (Investigator.resourcesL .~ 2)
    oops <- genPlayerCard Cards.oops
    shotgun4 <- buildAsset "01029"
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
        ((enemiesL %~ insertEntity enemy)
        . (enemiesL %~ insertEntity enemy2)
        . (locationsL %~ insertEntity location)
        . (assetsL %~ insertEntity shotgun4)
        )
      $ do
          runMessages
          [doFight] <- getAbilitiesOf shotgun4
          push $ UseAbility (toId investigator) doFight []
          runMessages
          chooseOptionMatching
            "fight enemy 1"
            (\case
              FightEnemy _ eid _ _ _ -> eid == toId enemy
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
              Run{} -> True
              _ -> False
            )
          updated enemy `shouldSatisfyM` hasDamage (0, 0)
          updated enemy2 `shouldSatisfyM` hasDamage (1, 0)
