module Arkham.Asset.Cards.JennysTwin45sSpec
  ( spec
  ) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Uses (useCount)
import Arkham.AssetId
import Arkham.Enemy.Attrs qualified as Enemy
import Arkham.Enemy.Attrs (Field(..))
import Arkham.Asset.Attrs (Field(..))
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Projection
import Arkham.Matcher (assetIs)

spec :: Spec
spec = describe "Jenny's Twin .45s" $ do
  it "enters play with X uses" $ do
    jennysTwin45s <- genPlayerCard Cards.jennysTwin45s
    investigator <- testInvestigator $ \attrs -> attrs
      { investigatorResources = 5
      , investigatorHand = [PlayerCard jennysTwin45s]
      }
    gameTest
        investigator
        [playDynamicCard investigator (PlayerCard jennysTwin45s) 5]
        id
      $ do
          runMessages
          pushAll
            [ PayedForDynamicCard (toId investigator) (toCardId jennysTwin45s) 5 True
            , PlayDynamicCard (toId investigator) (toCardId jennysTwin45s) 5 Nothing False
            ]
          runMessages
          assetId <- selectJust $ assetIs Cards.jennysTwin45s
          assert $ fieldP AssetUses ((== 5) . useCount) assetId

  it "gives +2 combat and does +1 damage" $ do
    jennysTwin45s <- genPlayerCard Cards.jennysTwin45s
    investigator <- testInvestigator $ \attrs -> attrs
      { investigatorResources = 1
      , investigatorHand = [PlayerCard jennysTwin45s]
      , investigatorCombat = 3
      }
    enemy <- testEnemy ((Enemy.healthL .~ Static 3) . (Enemy.fightL .~ 5))
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , playDynamicCard investigator (PlayerCard jennysTwin45s) 1
        ]
        ((entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          pushAll
            [ PayedForDynamicCard (toId investigator) (toCardId jennysTwin45s) 1 True
            , PlayDynamicCard (toId investigator) (toCardId jennysTwin45s) 1 Nothing False
            , enemySpawn location enemy
            , moveTo investigator location
            , UseCardAbility
              (toId investigator)
              (AssetSource . AssetId $ pcId jennysTwin45s)
              []
              1
              (UsesPayment 1)
            ]
          runMessages
          chooseOnlyOption "choose enemy"
          chooseOnlyOption "start skill test"
          chooseOnlyOption "apply results"
          assert $ fieldP EnemyDamage (== 2) (toId enemy)
