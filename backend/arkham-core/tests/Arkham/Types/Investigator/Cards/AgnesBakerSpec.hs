module Arkham.Types.Investigator.Cards.AgnesBakerSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Enemy.Attrs (EnemyAttrs(..))

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
          ((enemiesL %~ insertEntity enemy)
          . (locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
            chooseOnlyOption "damage enemy"
            updated enemy `shouldSatisfyM` hasDamage (1, 0)

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
          (locationsL %~ insertEntity location)
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"

            didPassTest `refShouldBe` True
