module Arkham.Event.Cards.EvidenceSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Event.Cards qualified as Events
import Arkham.Enemy.Types (healthL, fightL, Field(..))
import Arkham.Investigator.Types (combatL, resourcesL, Field(..))
import Arkham.Location.Types (cluesL, Field(..))
import Arkham.Projection

spec :: Spec
spec = describe
  "Evidence!"
  do
    it "discovers a clue at your location after you defeat an enemy" $ do
      investigator <- testJenny ((combatL .~ 1) . (resourcesL .~ 1))
      enemy <- testEnemy ((healthL .~ Static 1) . (fightL .~ 1))
      location <- testLocation (cluesL .~ 1)
      evidence <- genPlayerCard Events.evidence
      gameTest
          investigator
          [ SetTokens [Zero]
          , addToHand (toId investigator) (PlayerCard evidence)
          , enemySpawn location enemy
          , moveTo investigator location
          ]
          ((entitiesL . enemiesL %~ insertEntity enemy)
          . (entitiesL . locationsL %~ insertEntity location)
          )
        $ do
          runMessages
          (fight : _) <- field EnemyAbilities (toId enemy)
          pushAndRun $ UseAbility (toId investigator) fight []
          chooseOnlyOption "Begin skill test"
          chooseOnlyOption "Apply results"
          chooseOptionMatching "Play evidence" $ \case
            TargetLabel (CardIdTarget _) _ -> True
            _ -> False
          fieldAssert InvestigatorClues (== 1) investigator
          fieldAssert LocationClues (== 0) location
