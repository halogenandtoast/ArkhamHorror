module Arkham.Types.Asset.Cards.Rolands38SpecialSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Enemy.Attrs (EnemyAttrs(..))
import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Types.Location.Attrs (LocationAttrs(..))

spec :: Spec
spec = describe "Roland's .39 Special" $ do
  it "gives +1 combat and +1 damage" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorCombat = 1 }
    rolands38Special <- buildAsset "01006"
    enemy <- testEnemy
      $ \attrs -> attrs { enemyFight = 2, enemyHealth = Static 3 }
    location <- testLocation "00000" id
    runGameTest
        investigator
        [ SetTokens [Zero]
        , placedLocation location
        , enemySpawn location enemy
        , playAsset investigator rolands38Special
        , moveTo investigator location
        ]
        ((assetsL %~ insertEntity rolands38Special)
        . (enemiesL %~ insertEntity enemy)
        . (locationsL %~ insertEntity location)
        )
      $ do
          runMessagesNoLogging
          [fightAction] <- getActionsOf investigator NonFast rolands38Special
          runGameTestMessages [fightAction]
          runGameTestOnlyOption "choose enemy"
          runGameTestOnlyOption "start skill test"
          runGameTestOnlyOption "apply results"

          updated enemy `shouldSatisfyM` hasDamage (2, 0)

  it
      "gives +3 combat and +1 damage if there are 1 or more clues on your location"
    $ do
        investigator <- testInvestigator "00000"
          $ \attrs -> attrs { investigatorCombat = 1 }
        rolands38Special <- buildAsset "01006"
        enemy <- testEnemy
          $ \attrs -> attrs { enemyFight = 4, enemyHealth = Static 3 }
        location <- testLocation "00000" $ \attrs -> attrs { locationClues = 1 }
        runGameTest
            investigator
            [ SetTokens [Zero]
            , placedLocation location
            , enemySpawn location enemy
            , playAsset investigator rolands38Special
            , moveTo investigator location
            ]
            ((assetsL %~ insertEntity rolands38Special)
            . (enemiesL %~ insertEntity enemy)
            . (locationsL %~ insertEntity location)
            )
          $ do
              runMessagesNoLogging
              [fightAction] <- getActionsOf
                investigator
                NonFast
                rolands38Special
              runGameTestMessages [fightAction]
              runGameTestOnlyOption "choose enemy"
              runGameTestOnlyOption "start skill test"
              runGameTestOnlyOption "apply results"

              updated enemy `shouldSatisfyM` hasDamage (2, 0)
