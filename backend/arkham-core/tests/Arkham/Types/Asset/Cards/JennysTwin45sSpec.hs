module Arkham.Types.Asset.Cards.JennysTwin45sSpec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.AssetId
import qualified Arkham.Types.Enemy.Attrs as Enemy
import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

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
    runGameTest
        investigator
        [playDynamicCard investigator (PlayerCard jennysTwin45s)]
        id
      $ do
          runMessagesNoLogging
          payResource
          payResource
          payResource
          payResource
          payResource
          updatedJennysTwin45s <- getAsset (AssetId $ pcId jennysTwin45s)
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
    runGameTest
        investigator
        [ SetTokens [Zero]
        , playDynamicCard investigator (PlayerCard jennysTwin45s)
        ]
        ((enemiesL %~ insertEntity enemy)
        . (locationsL %~ insertEntity location)
        )
      $ do
          runMessagesNoLogging
          payResource
          runGameTestMessages
            [ enemySpawn location enemy
            , moveTo investigator location
            , UseCardAbility
              (toId investigator)
              (AssetSource . AssetId $ pcId jennysTwin45s)
              Nothing
              1
              (UsesPayment 1)
            ]
          runGameTestOnlyOption "choose enemy"
          runGameTestOnlyOption "start skill test"
          runGameTestOnlyOption "apply results"
          updated enemy `shouldSatisfyM` hasDamage (2, 0)
