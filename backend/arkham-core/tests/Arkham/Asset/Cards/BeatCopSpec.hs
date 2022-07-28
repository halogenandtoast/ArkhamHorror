module Arkham.Asset.Cards.BeatCopSpec
  ( spec
  ) where

import TestImport.Lifted hiding ( EnemyDamage )

import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Investigator ( modifiedStatsOf )
import Arkham.Investigator.Attrs ( InvestigatorAttrs (..) )
import Arkham.Enemy.Attrs ( Field(..), EnemyAttrs (..) )
import Arkham.Asset.Types ( Field(..) )
import Arkham.Projection

spec :: Spec
spec = describe "Beat Cop" $ do
  it "gives you +1 combat" $ do
    investigator <- testJenny
      $ \attrs -> attrs { investigatorCombat = 1 }
    beatCop <- buildAsset Assets.beatCop (Just investigator)
    gameTest
        investigator
        [playAsset investigator beatCop]
        (entitiesL . assetsL %~ insertEntity beatCop)
      $ do
          runMessages
          stats <- modifiedStatsOf
            (toSource investigator)
            Nothing
            (toId investigator)
          combat stats `shouldBe` 2

  it "can be discarded to do 1 damage to an enemy at your location" $ do
    investigator <- testJenny
      $ \attrs -> attrs { investigatorCombat = 1 }
    beatCop <- buildAsset Assets.beatCop (Just investigator)
    enemy <- testEnemy
      $ \attrs -> attrs { enemyHealth = Static 2 }
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , placedLocation location
        , enemySpawn location enemy
        , playAsset investigator beatCop
        , moveTo investigator location
        ]
        ((entitiesL . assetsL %~ insertEntity beatCop)
        . (entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          [discardAbility] <- field AssetAbilities (toId beatCop)
          pushAndRun $ UseAbility (toId investigator) discardAbility []
          fieldAssert EnemyDamage (== 1) enemy
