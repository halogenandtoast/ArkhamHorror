module Arkham.Types.Event.Cards.CloseCall2Spec
  ( spec
  ) where

import TestImport.Lifted

import qualified Arkham.Enemy.Cards as Cards
import qualified Arkham.Event.Cards as Cards
import qualified Arkham.Types.Investigator.Attrs as Investigator

spec :: Spec
spec = describe "Close Call (2)" $ do
  it "shuffles the enemy just evaded back into the encounter deck" $ do
    investigator <- testInvestigator "00000" (Investigator.resourcesL .~ 2)
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
        ((enemiesL %~ insertEntity enemy)
        . (locationsL %~ insertEntity location)
        )
      $ do
          runMessages
          chooseOptionMatching
            "Play card"
            (\case
              Run{} -> True
              _ -> False
            )
          game <- getTestGame
          length (game ^. encounterDeckL . to unDeck) `shouldBe` 1
          length (game ^. enemiesL) `shouldBe` 0

  it "does not work on Elite enemies" $ do
    investigator <- testInvestigator "00000" id
    closeCall2 <- genPlayerCard Cards.closeCall2
    enemy <- createEnemy <$> genEncounterCard Cards.ghoulPriest
    gameTest
        investigator
        [ addToHand investigator (PlayerCard closeCall2)
        , EnemyEvaded (toId investigator) (toId enemy)
        ]
        (enemiesL %~ insertEntity enemy)
      $ do
          runMessages
          queueRef <- view messageQueue
          queueRef `refShouldBe` []

  it "does not work on weakness enemies" $ do
    investigator <- testInvestigator "00000" id
    closeCall2 <- genPlayerCard Cards.closeCall2
    enemy <- createEnemy <$> genPlayerCard Cards.mobEnforcer
    gameTest
        investigator
        [ addToHand investigator (PlayerCard closeCall2)
        , EnemyEvaded (toId investigator) (toId enemy)
        ]
        (enemiesL %~ insertEntity enemy)
      $ do
          runMessages
          queueRef <- view messageQueue
          queueRef `refShouldBe` []
