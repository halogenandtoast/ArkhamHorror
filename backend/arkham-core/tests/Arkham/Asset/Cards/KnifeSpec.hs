module Arkham.Asset.Cards.KnifeSpec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Enemy.Attrs (EnemyAttrs(..))
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = describe "Knife" $ do
  it "Fight. You get +1 for this attack." $ do
    knife <- buildAsset "01086"
    investigator <- testInvestigator
      $ \attrs -> attrs { investigatorCombat = 2 }
    enemy <- testEnemy
      $ \attrs -> attrs { enemyHealth = Static 3, enemyFight = 3 }
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator knife
        , enemySpawn location enemy
        , moveTo investigator location
        ]
        ((entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . assetsL %~ insertEntity knife)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
        runMessages
        [knifeFightAction, _] <- getAbilitiesOf knife
        push $ UseAbility (toId investigator) knifeFightAction []
        runMessages
        chooseOnlyOption "Fight enemy"
        chooseOnlyOption "Start skill test"
        chooseOnlyOption "Apply Results"
        updated enemy `shouldSatisfyM` hasDamage (1, 0)
        isInDiscardOf investigator knife `shouldReturn` False

  it "Discard Knife: Fight. You get +2 for this attack. This attack deals +1 damage." $ do
    knife <- buildAsset "01086"
    investigator <- testInvestigator
      $ \attrs -> attrs { investigatorCombat = 1 }
    enemy <- testEnemy
      $ \attrs -> attrs { enemyHealth = Static 3, enemyFight = 3 }
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , playAsset investigator knife
        , enemySpawn location enemy
        , moveTo investigator location
        ]
        ((entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . assetsL %~ insertEntity knife)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
        runMessages
        [_, knifeDiscardFightAction] <- getAbilitiesOf knife
        push $ UseAbility (toId investigator) knifeDiscardFightAction []
        runMessages
        chooseOnlyOption "Fight enemy"
        chooseOnlyOption "Start skill test"
        chooseOnlyOption "Apply Results"
        updated enemy `shouldSatisfyM` hasDamage (2, 0)
        isInDiscardOf investigator knife `shouldReturn` True
