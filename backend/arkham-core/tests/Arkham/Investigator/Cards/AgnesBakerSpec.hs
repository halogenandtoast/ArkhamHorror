module Arkham.Investigator.Cards.AgnesBakerSpec
  ( spec
  ) where

import TestImport hiding (EnemyDamage)

import Arkham.Enemy.Attrs (Field (..), EnemyAttrs(..))

spec :: Spec
spec = describe "Agnes Baker" $ do
  context "ability" $ do
    it "can deal 1 damage to an enemy at your location when taking horror" $ do
      let agnesBaker = lookupInvestigator "01004"
      enemy <- testEnemy $ \attrs -> attrs { enemyHealth = Static 2 }
      location <- testLocation id
      gameTest
          agnesBaker
          [ placedLocation location
          , enemySpawn location enemy
          , moveTo agnesBaker location
          , InvestigatorDirectDamage (toId agnesBaker) (TestSource mempty) 0 1
          ]
          ((entitiesL . enemiesL %~ insertEntity enemy)
          . (entitiesL . locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOnlyOption "apply damage"
            chooseOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
            chooseOnlyOption "damage enemy"
            fieldAssert EnemyDamage (== 1) enemy

  context "elder sign" $ do
    it "gives +1 for each horror on Agnes" $ do
      let agnesBaker = lookupInvestigator "01004"
      location <- testLocation id

      (didPassTest, logger) <- didPassSkillTestBy agnesBaker SkillIntellect 0

      gameTestWithLogger
          logger
          agnesBaker
          [ SetTokens [ElderSign]
          , placedLocation location
          , moveTo agnesBaker location
          , InvestigatorDirectDamage (toId agnesBaker) (TestSource mempty) 0 2
          , beginSkillTest agnesBaker SkillIntellect 4
          ]
          (entitiesL . locationsL %~ insertEntity location)
        $ do
            runMessages
            chooseOnlyOption "apply damage"
            chooseOnlyOption "apply damage"
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"

            didPassTest `refShouldBe` True
