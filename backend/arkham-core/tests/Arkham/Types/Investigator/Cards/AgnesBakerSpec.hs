module Arkham.Types.Investigator.Cards.AgnesBakerSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Enemy.Attrs (Attrs(..))
import Arkham.Types.Token

spec :: Spec
spec = describe "Agnes Baker" $ do
  context "ability" $ do
    it "can deal 1 damage to an enemy at your location when taking horror" $ do
      let agnesBaker = lookupInvestigator "01004"
      enemy <- testEnemy $ \attrs -> attrs { enemyHealth = Static 2 }
      location <- testLocation "00000" id
      game <-
        runGameTest
          agnesBaker
          [ PlacedLocation (getId () location)
          , enemySpawn location enemy
          , moveTo agnesBaker location
          , InvestigatorDirectDamage (getId () agnesBaker) TestSource 0 1
          ]
          ((enemies %~ insertEntity enemy)
          . (locations %~ insertEntity location)
          )
        >>= runGameTestOptionMatching
              "use ability"
              (\case
                Run{} -> True
                _ -> False
              )
        >>= runGameTestOnlyOption "damage enemy"
      updated game enemy `shouldSatisfy` hasDamage (1, 0)

  context "elder sign" $ do
    it "gives +1 for each horror on Agnes" $ do
      let agnesBaker = lookupInvestigator "01004"
      location <- testLocation "00000" id
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          agnesBaker
          [ SetTokens [ElderSign]
          , PlacedLocation (getId () location)
          , moveTo agnesBaker location
          , InvestigatorDirectDamage (getId () agnesBaker) TestSource 0 2
          , beginSkillTest agnesBaker SkillIntellect 4
          ]
          ((scenario ?~ scenario') . (locations %~ insertEntity location))
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOption "apply results"
      game `shouldSatisfy` hasProcessedMessage
        (PassedSkillTest
          (getId () agnesBaker)
          Nothing
          TestSource
          SkillTestInitiatorTarget
          0
        )
