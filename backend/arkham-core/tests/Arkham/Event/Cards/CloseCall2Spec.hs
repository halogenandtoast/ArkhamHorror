module Arkham.Event.Cards.CloseCall2Spec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Event.Cards qualified as Cards
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Scenario.Types (Field(..))
import Arkham.GameEnv

spec :: Spec
spec = describe "Close Call (2)" $ do
  it "shuffles the enemy just evaded back into the encounter deck" $ do
    investigator <- testJenny (Investigator.resourcesL .~ 2)
    closeCall2 <- genCard Cards.closeCall2
    enemy <- testEnemy id
    location <- testLocation id
    gameTest
        investigator
        [ addToHand (toId investigator) closeCall2
        , placedLocation location
        , enemySpawn location enemy
        , moveTo investigator location
        , EnemyEvaded (toId investigator) (toId enemy)
        ]
        ((entitiesL . enemiesL %~ insertEntity enemy)
        . (entitiesL . locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          chooseOptionMatching
            "Play card"
            (\case
              TargetLabel{} -> True
              _ -> False
            )
          game <- getGame
          deckSize <- scenarioFieldMap ScenarioEncounterDeck length
          deckSize `shouldBe` (1 :: Int)
          length (game ^. entitiesL . enemiesL) `shouldBe` 0

  it "does not work on Elite enemies" $ do
    location <- testLocation id
    investigator <- testJenny id
    closeCall2 <- genCard Cards.closeCall2
    enemy <- createEnemy <$> genCard Cards.ghoulPriest <*> getRandom
    gameTest
        investigator
        [ moveTo investigator location
        , enemySpawn location enemy
        , addToHand (toId investigator) closeCall2
        , EnemyEvaded (toId investigator) (toId enemy)
        ]
        ( (entitiesL . locationsL %~ insertEntity location)
        . (entitiesL . enemiesL %~ insertEntity enemy)
        )
      $ do
          runMessages
          queueRef <- queueToRef <$> messageQueue
          queueRef `refShouldBe` []

  it "does not work on weakness enemies" $ do
    location <- testLocation id
    investigator <- testJenny id
    closeCall2 <- genCard Cards.closeCall2
    enemy <- createEnemy <$> genCard Cards.mobEnforcer <*> getRandom
    gameTest
        investigator
        [ SetBearer (toTarget enemy) (toId investigator)
        , moveTo investigator location
        , enemySpawn location enemy
        , addToHand (toId investigator) closeCall2
        , EnemyEvaded (toId investigator) (toId enemy)
        ]
        ( (entitiesL . locationsL %~ insertEntity location)
        . (entitiesL . enemiesL %~ insertEntity enemy)
        )
      $ do
          runMessages
          queueRef <- queueToRef <$> messageQueue
          queueRef `refShouldBe` []
