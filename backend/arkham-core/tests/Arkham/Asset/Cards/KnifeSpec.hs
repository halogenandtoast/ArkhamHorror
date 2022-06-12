module Arkham.Asset.Cards.KnifeSpec
  ( spec
  ) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Asset.Attrs ( Field(..) )
import Arkham.Enemy.Attrs ( Field(..), EnemyAttrs (..) )
import Arkham.Investigator.Attrs ( Field(..), InvestigatorAttrs (..) )
import Arkham.Projection

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
          [knifeFightAction, _] <- field AssetAbilities (toId knife)
          push $ UseAbility (toId investigator) knifeFightAction []
          runMessages
          chooseOnlyOption "Fight enemy"
          chooseOnlyOption "Start skill test"
          chooseOnlyOption "Apply Results"
          assert $ fieldP EnemyDamage (== 2) (toId enemy)
          assert $ fieldP InvestigatorDiscard null (toId investigator)

  it
      "Discard Knife: Fight. You get +2 for this attack. This attack deals +1 damage."
    $ do
        knife <- buildAsset "01086"
        let Just knifeCard = preview _PlayerCard (toCard $ toAttrs knife)
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
              [_, knifeDiscardFightAction] <- field AssetAbilities (toId knife)
              push $ UseAbility (toId investigator) knifeDiscardFightAction []
              runMessages
              chooseOnlyOption "Fight enemy"
              chooseOnlyOption "Start skill test"
              chooseOnlyOption "Apply Results"
              assert $ fieldP EnemyDamage (== 2) (toId enemy)
              assert $ fieldP InvestigatorDiscard (== [knifeCard]) (toId investigator)
