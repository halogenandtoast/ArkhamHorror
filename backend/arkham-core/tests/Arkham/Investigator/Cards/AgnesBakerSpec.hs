module Arkham.Investigator.Cards.AgnesBakerSpec (
  spec,
) where

import TestImport hiding (EnemyDamage)

import Arkham.Enemy.Types (EnemyAttrs (..), Field (..))
import Arkham.Investigator.Cards qualified as Investigators

spec :: Spec
spec = describe "Agnes Baker" $ do
  context "ability" $ do
    it "can deal 1 damage to an enemy at your location when taking horror" $ gameTestWith Investigators.agnesBaker $ \agnesBaker -> do
      enemy <- testEnemy $ \attrs -> attrs {enemyHealth = Static 2}
      location <- testLocation id
      pushAndRunAll
        [ placedLocation location
        , enemySpawn location enemy
        , moveTo agnesBaker location
        , InvestigatorDirectDamage (toId agnesBaker) (TestSource mempty) 0 1
        ]
      chooseOnlyOption "apply damage"
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOnlyOption "damage enemy"
      fieldAssert EnemyDamage (== 1) enemy

  context "elder sign" $ do
    it "gives +1 for each horror on Agnes" $ gameTestWith Investigators.agnesBaker $ \agnesBaker -> do
      location <- testLocation id

      didPassTest <- didPassSkillTestBy agnesBaker SkillIntellect 0

      pushAndRunAll
        [ SetChaosTokens [ElderSign]
        , placedLocation location
        , moveTo agnesBaker location
        , InvestigatorDirectDamage (toId agnesBaker) (TestSource mempty) 0 2
        , beginSkillTest agnesBaker SkillIntellect 4
        ]
      chooseOnlyOption "apply damage"
      chooseOnlyOption "apply damage"
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"

      didPassTest `refShouldBe` True
