module Arkham.Asset.Cards.Rolands38SpecialSpec
  ( spec
  ) where

import TestImport hiding (EnemyDamage)

import Arkham.Asset.Attrs (Field(..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Attrs (Field(..), EnemyAttrs(..))
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Location.Attrs (LocationAttrs(..))
import Arkham.Projection

spec :: Spec
spec = describe "Roland's .39 Special" $ do
  it "gives +1 combat and +1 damage" $ do
    investigator <- testInvestigator
      $ \attrs -> attrs { investigatorCombat = 1 }
    rolands38Special <- buildAsset Assets.rolands38Special (Just investigator)
    enemy <- testEnemy
      $ \attrs -> attrs { enemyFight = 2, enemyHealth = Static 3 }
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , placedLocation location
        , enemySpawn location enemy
        , playAsset investigator rolands38Special
        , moveTo investigator location
        ]
        ((entitiesL . assetsL %~ insertEntity rolands38Special)
        . (entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          [doFight] <- field AssetAbilities (toId rolands38Special)
          push $ UseAbility (toId investigator) doFight []
          runMessages
          chooseOnlyOption "choose enemy"
          chooseOnlyOption "start skill test"
          chooseOnlyOption "apply results"

          fieldAssert EnemyDamage (== 2) enemy

  it
      "gives +3 combat and +1 damage if there are 1 or more clues on your location"
    $ do
        investigator <- testInvestigator
          $ \attrs -> attrs { investigatorCombat = 1 }
        rolands38Special <- buildAsset Assets.rolands38Special (Just investigator)
        enemy <- testEnemy
          $ \attrs -> attrs { enemyFight = 4, enemyHealth = Static 3 }
        location <- testLocation $ \attrs -> attrs { locationClues = 1 }
        gameTest
            investigator
            [ SetTokens [Zero]
            , placedLocation location
            , enemySpawn location enemy
            , playAsset investigator rolands38Special
            , moveTo investigator location
            ]
            ((entitiesL . assetsL %~ insertEntity rolands38Special)
            . (entitiesL . enemiesL %~ insertEntity enemy)
            . (entitiesL . locationsL %~ insertEntity location)
            )
          $ do
              runMessages
              [doFight] <- field AssetAbilities (toId rolands38Special)
              push $ UseAbility (toId investigator) doFight []
              runMessages
              chooseOnlyOption "choose enemy"
              chooseOnlyOption "start skill test"
              chooseOnlyOption "apply results"
              fieldAssert EnemyDamage (== 2) enemy
