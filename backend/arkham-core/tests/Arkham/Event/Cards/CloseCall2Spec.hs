module Arkham.Event.Cards.CloseCall2Spec (
  spec,
) where

import TestImport.Lifted

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Event.Cards qualified as Cards
import Arkham.Investigator.Types qualified as Investigator
import Arkham.Matcher (EnemyMatcher (AnyEnemy))
import Arkham.Scenario.Types (Field (..))
import Arkham.Token

spec :: Spec
spec = describe "Close Call (2)" $ do
  it "shuffles the enemy just evaded back into the encounter deck" $ gameTest $ \investigator -> do
    updateInvestigator investigator (Investigator.tokensL %~ setTokens Resource 2)
    closeCall2 <- genCard Cards.closeCall2
    enemy <- testEnemyWith id
    location <- testLocationWith id
    pushAndRunAll
      [ addToHand (toId investigator) closeCall2
      , placedLocation location
      , spawnAt enemy location
      , moveTo investigator location
      , EnemyEvaded (toId investigator) (toId enemy)
      ]
    chooseOptionMatching
      "Play card"
      ( \case
          TargetLabel {} -> True
          _ -> False
      )
    deckSize <- scenarioFieldMap ScenarioEncounterDeck length
    deckSize `shouldBe` (1 :: Int)
    selectCount AnyEnemy `shouldReturn` 0

  it "does not work on Elite enemies" $ gameTest $ \investigator -> do
    location <- testLocationWith id
    closeCall2 <- genCard Cards.closeCall2
    enemy <- testEnemyWithDef Cards.ghoulPriest id
    pushAndRunAll
      [ moveTo investigator location
      , spawnAt enemy location
      , addToHand (toId investigator) closeCall2
      , EnemyEvaded (toId investigator) (toId enemy)
      ]
    queueRef <- queueToRef <$> messageQueue
    queueRef `refShouldBe` []

  it "does not work on weakness enemies" $ gameTest $ \investigator -> do
    location <- testLocationWith id
    closeCall2 <- genCard Cards.closeCall2
    enemy <- testEnemyWithDef Cards.mobEnforcer id
    pushAndRunAll
      [ SetBearer (toTarget enemy) (toId investigator)
      , moveTo investigator location
      , spawnAt enemy location
      , addToHand (toId investigator) closeCall2
      , EnemyEvaded (toId investigator) (toId enemy)
      ]
    queueRef <- queueToRef <$> messageQueue
    queueRef `refShouldBe` []
