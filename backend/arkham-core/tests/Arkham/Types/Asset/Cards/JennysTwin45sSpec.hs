module Arkham.Types.Asset.Cards.JennysTwin45sSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as Enemy
import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = describe "Jenny's Twin .45s" $ do
  let
    payResource = runGameTestOptionMatching
      "pay 1 resource"
      (\case
        Label "Increase spent resources" _ -> True
        _ -> False
      )

  it "enters play with X uses" $ do
    jennysTwin45s <- buildPlayerCard "02010"
    investigator <- testInvestigator "00000" $ \attrs -> attrs
      { investigatorResources = 5
      , investigatorHand = [PlayerCard jennysTwin45s]
      }
    game <-
      runGameTest
        investigator
        [playDynamicCard investigator (PlayerCard jennysTwin45s)]
        id
      >>= payResource
      >>= payResource
      >>= payResource
      >>= payResource
      >>= payResource
    let updatedJennysTwin45s = game ^?! assets . to toList . ix 0
    updatedJennysTwin45s `shouldSatisfy` hasUses 5
  it "gives +2 combat and does +1 damage" $ do
    jennysTwin45s <- buildPlayerCard "02010"
    investigator <- testInvestigator "00000" $ \attrs -> attrs
      { investigatorResources = 1
      , investigatorHand = [PlayerCard jennysTwin45s]
      , investigatorCombat = 3
      }
    enemy <- testEnemy ((Enemy.healthL .~ Static 3) . (Enemy.fightL .~ 5))
    location <- testLocation "00000" id
    game <-
      runGameTest
          investigator
          [ SetTokens [Zero]
          , playDynamicCard investigator (PlayerCard jennysTwin45s)
          ]
          ((enemies %~ insertEntity enemy)
          . (locations %~ insertEntity location)
          )
        >>= payResource
    let jennysTwin45sAsset = game ^?! assets . to toList . ix 0
    game' <-
      runGameTestMessages
        game
        [ enemySpawn location enemy
        , moveTo investigator location
        , UseCardAbility
          (toId investigator)
          (toSource jennysTwin45sAsset)
          Nothing
          1
        ]
      >>= runGameTestOnlyOption "choose enemy"
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOnlyOption "apply results"
    updated game' enemy `shouldSatisfy` hasDamage (2, 0)
