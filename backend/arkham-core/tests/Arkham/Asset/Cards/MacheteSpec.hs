module Arkham.Asset.Cards.MacheteSpec
  ( spec
  ) where

import TestImport hiding (EnemyDamage)

import Arkham.Asset.Attrs (Field(..))
import Arkham.Enemy.Attrs (Field(..), EnemyAttrs(..))
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Projection

spec :: Spec
spec = describe "Machete" $ do
  it "gives +1 combat and +1 damage if the attacked enemy is the only enemy engaged with you" $ do
    investigator <- testInvestigator
      $ \attrs -> attrs { investigatorCombat = 1 }
    machete <- buildAsset "01016" (Just investigator)
    enemy <- testEnemy
      $ \attrs -> attrs { enemyFight = 2, enemyHealth = Static 3 }
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , placedLocation location
        , enemySpawn location enemy
        , playAsset investigator machete
        , moveTo investigator location
        ]
        ((entitiesL . assetsL %~ insertEntity machete)
        . (entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          [doFight] <- field AssetAbilities (toId machete)
          push $ UseAbility (toId investigator) doFight []
          runMessages
          chooseOnlyOption "choose enemy"
          chooseOnlyOption "start skill test"
          chooseOnlyOption "apply results"

          fieldAssert EnemyDamage (== 2) enemy

  fit "does not give additional damage if the attacked enemy is not engaged with you" $ do
    investigator <- testInvestigator
      $ \attrs -> attrs { investigatorCombat = 1 }
    machete <- buildAsset "01016" (Just investigator)
    enemy <- testEnemy
      $ \attrs -> attrs { enemyFight = 2, enemyHealth = Static 3, enemyExhausted = True }
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , placedLocation location
        , enemySpawn location enemy
        , playAsset investigator machete
        , moveTo investigator location
        ]
        ((entitiesL . assetsL %~ insertEntity machete)
        . (entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          [doFight] <- field AssetAbilities (toId machete)
          push $ UseAbility (toId investigator) doFight []
          runMessages
          chooseOnlyOption "choose enemy"
          chooseOnlyOption "start skill test"
          chooseOnlyOption "apply results"

          fieldAssert EnemyDamage (== 1) enemy
