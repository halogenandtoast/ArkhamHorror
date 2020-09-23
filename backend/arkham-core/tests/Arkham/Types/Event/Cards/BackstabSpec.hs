module Arkham.Types.Event.Cards.BackstabSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Difficulty
import Arkham.Types.Helpers
import Arkham.Types.Token

spec :: Spec
spec = do
  describe "Backstab" $ do
    it "should use agility and do +2 damage" $ do
      theGathering <- newScenario Easy "01104"
      (locationId, study) <- newLocation "01111"
      (investigatorId, investigator) <- newInvestigator "00000"
        $ \stats -> stats { combat = 1, agility = 4 }
      (eventId, backstab) <- newEvent "01051" investigatorId
      (enemyId, ravenousGhoul) <- newEnemy "01161"
      -- Ravenous Ghoul has 3 health
      game <-
        runGameTest
          investigator
          [ EnemySpawn locationId enemyId
          , MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId eventId
          ]
          ((events %~ insertMap eventId backstab)
          . (locations %~ insertMap locationId study)
          . (enemies %~ insertMap enemyId ravenousGhoul)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ theGathering)
          )
        >>= runGameTestOnlyOption "Fight enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      -- We expect the skill check to succeed and the enemy to be defeated
      ravenousGhoul `shouldSatisfy` isInEncounterDiscard game
      backstab `shouldSatisfy` isInDiscardOf game investigator
