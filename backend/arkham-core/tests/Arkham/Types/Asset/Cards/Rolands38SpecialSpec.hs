module Arkham.Types.Asset.Cards.Rolands38SpecialSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Enemy.Attrs (Attrs(..))
import Arkham.Types.Investigator.Attrs (Attrs(..))
import Arkham.Types.Location.Attrs (Attrs(..))

spec :: Spec
spec = describe "Roland's .39 Special" $ do
  it "gives +1 combat and +1 damage" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorCombat = 1 }
    rolands38Special <- buildAsset "01006"
    enemy <- testEnemy
      $ \attrs -> attrs { enemyFight = 2, enemyHealth = Static 3 }
    location <- testLocation "00000" id
    game <- runGameTest
      investigator
      [ SetTokens [Zero]
      , PlacedLocation (getLocationId location)
      , enemySpawn location enemy
      , playAsset investigator rolands38Special
      , moveTo investigator location
      ]
      ((assetsL %~ insertEntity rolands38Special)
      . (enemiesL %~ insertEntity enemy)
      . (locationsL %~ insertEntity location)
      )

    [fightAction] <- getActionsOf game investigator NonFast rolands38Special

    game' <-
      runGameTestMessages game [fightAction]
      >>= runGameTestOnlyOption "choose enemy"
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOnlyOption "apply results"

    updated game' enemy `shouldSatisfy` hasDamage (2, 0)
  it
      "gives +3 combat and +1 damage if there are 1 or more clues on your location"
    $ do
        investigator <- testInvestigator "00000"
          $ \attrs -> attrs { investigatorCombat = 1 }
        rolands38Special <- buildAsset "01006"
        enemy <- testEnemy
          $ \attrs -> attrs { enemyFight = 4, enemyHealth = Static 3 }
        location <- testLocation "00000" $ \attrs -> attrs { locationClues = 1 }
        game <- runGameTest
          investigator
          [ SetTokens [Zero]
          , PlacedLocation (getLocationId location)
          , enemySpawn location enemy
          , playAsset investigator rolands38Special
          , moveTo investigator location
          ]
          ((assetsL %~ insertEntity rolands38Special)
          . (enemiesL %~ insertEntity enemy)
          . (locationsL %~ insertEntity location)
          )

        [fightAction] <- getActionsOf game investigator NonFast rolands38Special

        game' <-
          runGameTestMessages game [fightAction]
          >>= runGameTestOnlyOption "choose enemy"
          >>= runGameTestOnlyOption "start skill test"
          >>= runGameTestOnlyOption "apply results"

        updated game' enemy `shouldSatisfy` hasDamage (2, 0)
