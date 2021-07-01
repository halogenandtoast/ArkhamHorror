module Arkham.Types.Event.Cards.CloseCall2Spec
  ( spec
  )
where

import TestImport.Lifted

import qualified Arkham.Types.Card.CardDef as CardDef
import Arkham.Types.Trait

spec :: Spec
spec = describe "Close Call (2)" $ do
  it "shuffles the enemy just evaded back into the encounter deck" $ do
    investigator <- testInvestigator "00000" id
    closeCall2 <- buildPlayerCard "01083"
    enemy <- testEnemy id
    gameTest
        investigator
        [ addToHand investigator (PlayerCard closeCall2)
        , EnemyEvaded (toId investigator) (toId enemy)
        ]
        (enemiesL %~ insertEntity enemy)
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
    closeCall2 <- buildPlayerCard "01083"
    enemy <- testEnemyWithDef (CardDef.cardTraitsL .~ setFromList [Elite]) id
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
    closeCall2 <- buildPlayerCard "01083"
    enemy <- testWeaknessEnemy id
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
