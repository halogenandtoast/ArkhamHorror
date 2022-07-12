module Arkham.Event.Cards.CloseCall2Spec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Event.Cards qualified as Cards
import Arkham.Investigator.Attrs qualified as Investigator
import Arkham.Scenario.Attrs (Field(..))
import Arkham.GameEnv

spec :: Spec
spec = describe "Close Call (2)" $ do
  it "shuffles the enemy just evaded back into the encounter deck" $ do
    investigator <- testJenny (Investigator.resourcesL .~ 2)
    closeCall2 <- genPlayerCard Cards.closeCall2
    enemy <- testEnemy id
    location <- testLocation id
    gameTest
        investigator
        [ addToHand investigator (PlayerCard closeCall2)
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
          deckSize <- scenarioFieldMap ScenarioEncounterDeck (length . unDeck)
          deckSize `shouldBe` (1 :: Int)
          length (game ^. entitiesL . enemiesL) `shouldBe` 0

  it "does not work on Elite enemies" $ do
    location <- testLocation id
    investigator <- testJenny id
    closeCall2 <- genPlayerCard Cards.closeCall2
    enemy <- createEnemy <$> genEncounterCard Cards.ghoulPriest
    gameTest
        investigator
        [ moveTo investigator location
        , enemySpawn location enemy
        , addToHand investigator (PlayerCard closeCall2)
        , EnemyEvaded (toId investigator) (toId enemy)
        ]
        ( (entitiesL . locationsL %~ insertEntity location)
        . (entitiesL . enemiesL %~ insertEntity enemy)
        )
      $ do
          runMessages
          queueRef <- view messageQueue
          queueRef `refShouldBe` []

  it "does not work on weakness enemies" $ do
    location <- testLocation id
    investigator <- testJenny id
    closeCall2 <- genPlayerCard Cards.closeCall2
    enemy <- createEnemy <$> genPlayerCard Cards.mobEnforcer
    gameTest
        investigator
        [ SetBearer (toTarget enemy) (toId investigator)
        , moveTo investigator location
        , enemySpawn location enemy
        , addToHand investigator (PlayerCard closeCall2)
        , EnemyEvaded (toId investigator) (toId enemy)
        ]
        ( (entitiesL . locationsL %~ insertEntity location)
        . (entitiesL . enemiesL %~ insertEntity enemy)
        )
      $ do
          runMessages
          queueRef <- view messageQueue
          queueRef `refShouldBe` []
