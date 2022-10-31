module Arkham.Asset.Cards.JennysTwin45sSpec
  ( spec
  ) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.ActiveCost
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Uses (useCount)
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Enemy.Types (Field(..))
import Arkham.Asset.Types (Field(..))
import Arkham.Investigator.Types (InvestigatorAttrs(..))
import Arkham.Projection
import Arkham.Matcher (assetIs)

spec :: Spec
spec = describe "Jenny's Twin .45s" $ do
  it "enters play with X uses" $ do
    jennysTwin45s <- genPlayerCard Cards.jennysTwin45s
    investigator <- testJenny $ \attrs -> attrs
      { investigatorResources = 5
      , investigatorHand = [PlayerCard jennysTwin45s]
      }
    gameTest
        investigator
        [playCard investigator (PlayerCard jennysTwin45s)]
        id
      $ do
          runMessages
          activeCost <- getActiveCost
          pushAll
            $ replicate 5 (PayCost (activeCostId activeCost) (toId investigator) False (ResourceCost 1))
          runMessages
          assetId <- selectJust $ assetIs Cards.jennysTwin45s
          assert $ fieldP AssetUses ((== 5) . useCount) assetId

  it "gives +2 combat and does +1 damage" $ do
    jennysTwin45s <- genPlayerCard Cards.jennysTwin45s
    investigator <- testJenny $ \attrs -> attrs
      { investigatorResources = 1
      , investigatorHand = [PlayerCard jennysTwin45s]
      , investigatorCombat = 3
      }
    enemy <- testEnemy ((Enemy.healthL .~ Static 3) . (Enemy.fightL .~ 5))
    location <- testLocation id
    gameTest
        investigator
        [ SetTokens [Zero]
        , playCard investigator (PlayerCard jennysTwin45s)
        ]
        ((entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          activeCost <- getActiveCost
          push $
            PayCost (activeCostId activeCost) (toId investigator) False (ResourceCost 1)
          runMessages
          pushAll
            [ enemySpawn location enemy
            , moveTo investigator location
            , UseCardAbility
              (toId investigator)
              (AbilityRef (AssetSource . AssetId $ pcId jennysTwin45s) 1)
              []
              (UsesPayment 1)
            ]
          runMessages
          chooseOnlyOption "choose enemy"
          chooseOnlyOption "start skill test"
          chooseOnlyOption "apply results"
          assert $ fieldP EnemyDamage (== 2) (toId enemy)
