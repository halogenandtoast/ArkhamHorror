module Arkham.Asset.Cards.FortyFiveAutomaticSpec
  ( spec
  ) where

import TestImport hiding (EnemyDamage)

import Arkham.Asset.Types (Field(..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Types (Field(..), EnemyAttrs(..))
import Arkham.Investigator.Types (InvestigatorAttrs(..))
import Arkham.Projection

spec :: Spec
spec = describe ".45 Automatic" $ do
  it "gives +1 combat and +1 damage" $ do
    investigator <- testJenny
      $ \attrs -> attrs { investigatorCombat = 1 }
    fortyFiveAutomatic <- buildAsset Assets.fortyFiveAutomatic (Just investigator)
    enemy <- testEnemy
      $ \attrs -> attrs { enemyFight = 2, enemyHealth = Static 3 }
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , placedLocation location
        , enemySpawn location enemy
        , playAsset investigator fortyFiveAutomatic
        , moveTo investigator location
        ]
        ((entitiesL . assetsL %~ insertEntity fortyFiveAutomatic)
        . (entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          [doFight] <- field AssetAbilities (toId fortyFiveAutomatic)
          push $ UseAbility (toId investigator) doFight []
          runMessages
          chooseOnlyOption "choose enemy"
          chooseOnlyOption "start skill test"
          chooseOnlyOption "apply results"

          fieldAssert EnemyDamage (== 2) enemy
