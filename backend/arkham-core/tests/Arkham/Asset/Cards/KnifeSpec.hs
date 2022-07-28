{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Arkham.Asset.Cards.KnifeSpec
  ( spec
  ) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Asset.Types ( Field(..) )
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Types ( Field(..), EnemyAttrs (..) )
import Arkham.Investigator.Types ( Field(..), InvestigatorAttrs (..) )
import Arkham.Projection

spec :: Spec
spec = describe "Knife" $ do
  it "Fight. You get +1 for this attack." $ do
    investigator <- testJenny
      $ \attrs -> attrs { investigatorCombat = 2 }
    knife <- buildAsset Assets.knife (Just investigator)
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
          [knifeFightAction, _] <- field AssetAbilities (toId knife)
          push $ UseAbility (toId investigator) knifeFightAction []
          runMessages
          chooseOnlyOption "Fight enemy"
          chooseOnlyOption "Start skill test"
          chooseOnlyOption "Apply Results"
          fieldAssert EnemyDamage (== 1) enemy
          fieldAssert InvestigatorDiscard null investigator

  it
      "Discard Knife: Fight. You get +2 for this attack. This attack deals +1 damage."
    $ do
        investigator <- testJenny
          $ \attrs -> attrs { investigatorCombat = 1 }
        knife <- buildAsset Assets.knife (Just investigator)
        let Just knifeCard = preview _PlayerCard (toCard $ toAttrs knife)
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
              [_, knifeDiscardFightAction] <- field AssetAbilities (toId knife)
              push $ UseAbility (toId investigator) knifeDiscardFightAction []
              runMessages
              chooseOnlyOption "Fight enemy"
              chooseOnlyOption "Start skill test"
              chooseOnlyOption "Apply Results"
              fieldAssert EnemyDamage (== 2) enemy
              fieldAssert InvestigatorDiscard (== [knifeCard]) investigator
