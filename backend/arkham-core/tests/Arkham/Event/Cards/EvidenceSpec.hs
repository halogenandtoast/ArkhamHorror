module Arkham.Event.Cards.EvidenceSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Enemy.Types (Field (..), fightL, healthL)
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types (Field (..), combatL, resourcesL)
import Arkham.Location.Types (Field (..), revealCluesL)
import Arkham.Projection

spec :: Spec
spec = describe
  "Evidence!"
  do
    it "discovers a clue at your location after you defeat an enemy" $ gameTest $ \investigator -> do
      updateInvestigator investigator ((combatL .~ 1) . (resourcesL .~ 1))
      enemy <- testEnemy ((healthL .~ Static 1) . (fightL .~ 1))
      location <- testLocation (revealCluesL .~ Static 1)
      evidence <- genCard Events.evidence
      pushAndRunAll
        [ SetChaosTokens [Zero]
        , addToHand (toId investigator) evidence
        , enemySpawn location enemy
        , moveTo investigator location
        ]
      (fight : _) <- field EnemyAbilities (toId enemy)
      pushAndRun $ UseAbility (toId investigator) fight []
      chooseOnlyOption "Begin skill test"
      chooseOnlyOption "Apply results"
      chooseOptionMatching "Play evidence" $ \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      fieldAssert InvestigatorClues (== 1) investigator
      fieldAssert LocationClues (== 0) location
