module Arkham.Types.Event.Cards.CloseCall2Spec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.Enemy.Attrs (EnemyAttrs(..))
import Arkham.Types.Trait

spec :: Spec
spec = describe "Close Call (2)" $ do
  it "shuffles the enemy just evaded back into the encounter deck" $ do
    investigator <- testInvestigator "00000" id
    closeCall2 <- buildPlayerCard "01083"
    enemy <- testEnemy id
    runGameTest
        investigator
        [ addToHand investigator (PlayerCard closeCall2)
        , EnemyEvaded (toId investigator) (toId enemy)
        ]
        (enemiesL %~ insertEntity enemy)
      $ do
          runMessagesNoLogging
          runGameTestOptionMatching
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
    closeCall2 <- buildPlayerCard "01083"
    enemy <- testEnemy $ \attrs -> attrs { enemyTraits = setFromList [Elite] }
    runGameTest
        investigator
        [ addToHand investigator (PlayerCard closeCall2)
        , EnemyEvaded (toId investigator) (toId enemy)
        ]
        (enemiesL %~ insertEntity enemy)
      $ do
          runMessagesNoLogging
          queueRef <- view messageQueue
          queueRef `refShouldBe` []

  it "does not work on weakness enemies" $ do
    investigator <- testInvestigator "00000" id
    closeCall2 <- buildPlayerCard "01083"
    enemy <- testEnemy $ \attrs -> attrs { enemyCardCode = "01102" } -- uses a card code for a weakness
    runGameTest
        investigator
        [ addToHand investigator (PlayerCard closeCall2)
        , EnemyEvaded (toId investigator) (toId enemy)
        ]
        (enemiesL %~ insertEntity enemy)
      $ do
          runMessagesNoLogging
          queueRef <- view messageQueue
          queueRef `refShouldBe` []
