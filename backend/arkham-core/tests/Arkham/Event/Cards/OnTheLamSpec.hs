module Arkham.Event.Cards.OnTheLamSpec (
  spec,
)
where

import TestImport

import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types

spec :: Spec
spec = describe "On The Lam" $ do
  it "prevents non-elite enemies from attacking you until the end of the round" . gameTest $ \investigator -> do
    ref <- createMessageChecker \case
      EnemyAttack {} -> True
      _ -> False
    updateInvestigator investigator $ \i -> i {investigatorResources = 1}
    onTheLam <- genCard Events.onTheLam
    location <- testLocation id
    pushAndRunAll
      [ addToHand (toId investigator) onTheLam
      , MoveAllTo GameSource (toId location)
      , BeginTurn (toId investigator)
      ]
    chooseFirstOption "Play on the lam"
    enemy <- testEnemy id
    pushAndRun $ enemySpawn location enemy
    pushAndRun $ TakeResources (toId investigator) 1 (toSource investigator) True
    ref `refShouldBe` False

    pushAndRunAll [ChooseEndTurn (toId investigator), EnemiesAttack]
    ref `refShouldBe` False

    pushAndRunAll [EndRound, EnemiesAttack]
    ref `refShouldBe` True
